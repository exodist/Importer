use strict;
use warnings;

use Importer 'Test::More';

my $CLASS = 'Importer';

sub dies(&) {
    my $code = shift;

    my $err;
    {
        local $@;
        eval { $code->(); 1 } and return undef;
        $err = $@;
    }

    $@ = $err;
    return $err || 1;
}

sub warns(&) {
    my $code = shift;

    my $warn;
    my $warned = 0;
    local $SIG{__WARN__} = sub { ($warn) = @_; $warned++ };
    $code->();

    return undef unless $warned;

    return $warn || $warned;
}

subtest _version_check => sub {
    my $version_check = $CLASS->can('_version_check');

    ok($version_check->('Importer', [__PACKAGE__, __FILE__, __LINE__], 0.001), "Version check pass" );

    my $error = dies { $version_check->('Importer', [__PACKAGE__, __FILE__, __LINE__], 100) };
    my $line = __LINE__ - 1;

    my $file = __FILE__;
    like(
        $error,
        qr/version 100 required.*at $file line $line/,
        "Got expected error"
    );
};

subtest import => sub {
    ok(!dies { $CLASS->import('0.001') }, "No errors, valid version");
    like(dies { $CLASS->import('100') }, qr/version 100 required/, "bad version check");

    package Consumer1;
    use Importer 'Data::Dumper' => 'Dumper';

    ::can_ok(__PACKAGE__, 'Dumper');
};

subtest import_into => sub {
    $CLASS->import_into('Data::Dumper', 'Consumer2', 'Dumper');
    can_ok('Consumer2', 'Dumper');

    my $do_it = sub { $CLASS->import_into('Data::Dumper', 0, 'Dumper') };
    package Consumer3;
    $do_it->();

    ::can_ok('Consumer3', 'Dumper');
};

subtest unimport => sub {
    package Consumer1;
    $CLASS->unimport;

    ::ok(!__PACKAGE__->can('Dumper'), "removed 'Dumper' from Consumer1");
    ::like(
        ::dies { $CLASS->unimport('foo') },
        qr/Sub 'foo' was not imported using Importer/,
        "we did not import it, we cannot remove it"
    );

    package Consumer100;

    ::like(
        ::dies { $CLASS->unimport },
        qr/'Consumer100' does not have any imports to remove/,
        "nothing to unimport"
    );
};

subtest unimport_from => sub {
    $CLASS->unimport_from('Consumer2', 'Dumper');
    ok(!Consumer2->can('Dumper'), "removed 'Dumper' from Consumer2");

    like(
        dies { $CLASS->unimport_from('Consumer100') },
        qr/'Consumer100' does not have any imports to remove/,
        "Nothing to unimport"
    );

    my $do_it  = sub { $CLASS->unimport_from(0, 'Dumper') };
    my $do_it2 = sub { $CLASS->unimport_from(0, 'foo') };

    package Consumer3;
    $do_it->();

    ::ok(!Consumer3->can('Dumper'), "removed 'Dumper' from Consumer3");

    ::like(
        ::dies { $do_it2->() },
        qr/Sub 'foo' was not imported using Importer/,
        "we did not import it, we cannot remove it"
    );
};

subtest new_and_from => sub {
    my $one = $CLASS->new(from => 'Data::Dumper');
    isa_ok($one, $CLASS);
    is($one->from, 'Data::Dumper', "Saved 'from' from constructor");

    like(
        dies { $CLASS->new() },
        qr/You must specify a package to import from/,
        "'from' is a required attribute"
    );

    $one = $CLASS->new(from => 'Data::Dumper', caller => ['Foo::Bar', 'Foo/Bar.pm', 42]);
    is_deeply($one->get_caller, ['Foo::Bar', 'Foo/Bar.pm', 42], "Saved caller from construction");
};

subtest from_file => sub {
    my $one = $CLASS->new(from => 'Foo::Bar::Baz');
    is($one->from_file, 'Foo/Bar/Baz.pm', "got filename");
};

subtest load_from => sub {
    my $one = $CLASS->new(from => 'Some::Fake::Module::AFSGEWGWE::FASDF', caller => ['main', 'fake.pl', 42]);

    like(
        dies { $one->load_from },
        qr{Can't locate.*at fake\.pl line 42},
        "Failed to load 'from' module",
    );

    $INC{$one->from_file} = 1;
    ok(!dies { $one->load_from }, "file already loaded");

    ok(!$INC{'Test/Simple.pm'}, "check that our test file is not already loaded");
    $one = $CLASS->new(from => 'Test::Simple');
    ok(!dies { $one->load_from }, "file found");
};

subtest get_caller => sub {
    my $one = $CLASS->new(from => 'Fake', caller => ['A', 'A.pm', 42]);
    is_deeply($one->get_caller, ['A', 'A.pm', 42], "got stored caller");

    $one = $CLASS->new(from => 'Fake');
    is_deeply([@{sub { $one->get_caller }->()}[0,1,2]], [__PACKAGE__, __FILE__, __LINE__], "got real caller");

    my $get = sub {
        package Importer;
        sub {
            package Importer::Subclass;
            use base 'Importer';
            sub {
                package main; # get_caller loosk at level 1+, so this gets skipped by design
                $one->get_caller;
            }->()
        }->()
    };

    is_deeply([@{$get->()}[0,1,2]], [__PACKAGE__, __FILE__, __LINE__], "got true caller");
};

subtest carp_and_croak => sub {
    my $one = $CLASS->new(from => 'fake', caller => ['A', 'A.pm', 42]);

    is(
        dies { $one->croak("apple pie") },
        "apple pie at A.pm line 42.\n",
        "Died at correct place"
    );

    is(
        warns { $one->carp("apple pie") },
        "apple pie at A.pm line 42.\n",
        "Warned at correct place"
    );
};

subtest menu => sub {
    my $menu;

    no warnings 'redefine';
    local *Importer::reload_menu = sub {
        my $self = shift;
        my ($into) = @_;
        $self->{menu} = $menu;
        $self->{menu_for} = $into;
        return $menu;
    };

    $menu = { a => 1 };
    my $one = $CLASS->new(from => 'fake');
    is_deeply($one->menu('fake2'), $menu, "returned menu");

    my $old = $menu;
    $menu = { b => 2 };

    is_deeply($one->menu('fake2'), $old, "cached");

    is_deeply($one->menu('fake3'), $menu, "refreshed with different destination");

    my $line;
    like(
        dies { $line = __LINE__; $one->menu() },
        qr/menu\(\) requires the name of the destination package at ${\__FILE__} line $line/,
        "Need 'into' package"
    );
};

subtest reload_menu => sub {
    my $one = $CLASS->new(from => 'fake');

    my $line;
    like(
        dies { $line = __LINE__; $one->reload_menu() },
        qr/menu\(\) requires the name of the destination package at ${\__FILE__} line $line/,
        "Need 'into' package"
    );

    subtest empty => sub {
        {
            no warnings 'once';
            require Exporter;
            @Fake::Exporter1::ISA = ('Exporter');
            *Fake::Exporter2::import = Exporter->can('import');
            *Fake::Exporter3::IMPORTER_MENU = sub { () };
            *Fake::Exporter4::IMPORTER_MENU = sub { (generate => sub { 1 }, export_gen => { a => 1 }) };
        }

        like(
            dies { $line = __LINE__; $CLASS->new(from => 'Fake::Exporter4')->reload_menu('fake') },
            qr/'Fake::Exporter4' provides both 'generate' and 'export_gen' in its IMPORTER_MENU \(They are exclusive, module must pick 1\) at ${\__FILE__} line $line/,
            "Bad IMPORT_MENU"
        );

        like(
            dies { $line = __LINE__; $CLASS->new(from => 'Fake::Exporter5')->reload_menu('fake') },
            qr/'Fake::Exporter5' does not provide any exports at ${\__FILE__} line $line/,
            "No exports, not an exporter"
        );

        my ($menu1, $menu2, $menu3);
        ok(!dies { $menu1 = $CLASS->new(from => 'Fake::Exporter1')->reload_menu('fake') }, "Package isa Exporter with no exports") || diag $@;
        ok(!dies { $menu2 = $CLASS->new(from => 'Fake::Exporter2')->reload_menu('fake') }, "Package uses Exporter qw/import/") || diag $@;
        ok(!dies { $menu3 = $CLASS->new(from => 'Fake::Exporter3')->reload_menu('fake') }, "Package provides IMPORTER_MENU") || diag $@;

        is_deeply(
            [$menu1, $menu1, $menu2],
            [$menu2, $menu3, $menu3],
            "All empty menus are the same"
        );

        is_deeply(
            $menu1,
            {
                lookup   => {},
                exports  => {},
                tags     => { DEFAULT => [] },
                fail     => undef,
                generate => undef,
            },
            "Got valid, but empty menu"
        );
    };

    subtest IMPORTER_MENU => sub {
        {
            package Fake::ExporterI;
            sub IMPORTER_MENU {
                ::is_deeply(
                    \@_,
                    ['Fake::ExporterI', 'fake', ['fake', 'fake.pl', 42]],
                    "Got input args"
                );
                return (
                    export      => [qw/foo &bar $ZAP %ZAP @ZAP/],
                    export_ok   => [qw/baz ick missing/],
                    export_tags => {b => [qw/bar baz/]},
                    export_fail => [qw/ick/],
                    export_anon => { x => \&__x, z => \&__z },
                    export_gen  => {
                        'gena' => sub {
                            sub { 'a' }
                        },
                        '&genb' => sub {
                            sub { 'b' }
                        },
                    },
                );
            }

            sub foo { 'foo' }
            sub bar { 'bar' }
            sub baz { 'baz' }
            sub ick { 'ick' }
            sub __x { 'x' }
            sub __z { 'z' }

            # These are here to insure 'exports' does not pull them in, they are listed as generate
            sub gena { sub { 'oops, should not see this' } }
            sub genb { sub { 'oops, should not see this' } }

            # These are here to insure 'exports' does not pull them in, refs were provided by anon
            sub x { sub { 'oops, should not see this' } }
            sub y { sub { 'oops, should not see this' } }

            package Fake::ExporterI2;

            sub IMPORTER_MENU {
                return (
                    generate => \&generate,
                );
            }

            sub generate { sub { 'a pie' } }
        }

        my $one = $CLASS->new(from => 'Fake::ExporterI', caller => ['fake', 'fake.pl', 42]);
        my $menu = $one->reload_menu('fake');
        is($one->{menu_for}, 'fake', "remember who it was generated for");
        ok(my $gen = delete $menu->{generate}, "got a generate function");

        is_deeply(
            $menu,
            {
                lookup => {qw/
                    foo 1       &foo 1
                    bar 1       &bar 1
                    baz 1       &baz 1
                    ick 1       &ick 1
                    missing 1   &missing 1
                    x 1         &x 1
                    z 1         &z 1
                    gena 1      &gena 1
                    genb 1      &genb 1

                    $ZAP 1 %ZAP 1 @ZAP 1
                /},
                exports => {
                    '&foo' => Fake::ExporterI->can('foo'),
                    '&bar' => Fake::ExporterI->can('bar'),
                    '&baz' => Fake::ExporterI->can('baz'),
                    '&ick' => Fake::ExporterI->can('ick'),
                    '&x' => Fake::ExporterI->can('__x'),
                    '&z' => Fake::ExporterI->can('__z'),

                    '&missing' => undef,

                    '$ZAP' => \$Fake::ExporterI::ZAP,
                    '@ZAP' => \@Fake::ExporterI::ZAP,
                    '%ZAP' => \%Fake::ExporterI::ZAP,
                },
                tags => {
                    b => [qw/bar baz/],
                    DEFAULT => [qw/foo &bar $ZAP %ZAP @ZAP/],
                },
                fail => { '&ick' => 1, ick => 1 },
            },
            "Got menu"
        );

        is($gen->('gena')->(), 'a', "generated a");
        is($gen->('genb')->(), 'b', "generated b");

        $one = $CLASS->new(from => 'Fake::ExporterI2', caller => ['fake', 'fake.pl', 42]);
        $menu = $one->reload_menu('fake');
        is($menu->{generate}, \&Fake::ExporterI2::generate, "can provide custom generate")
    };

    subtest OLD_STYLE => sub {
        {
            package Fake::ExporterE;
            our @EXPORT      = qw/foo &bar $ZAP %ZAP @ZAP/;
            our @EXPORT_OK   = qw/baz ick missing/;
            our %EXPORT_TAGS = (b => [qw/bar baz/]);
            our @EXPORT_FAIL = qw/ick/;
            our %EXPORT_ANON = (x => \&__x, z => \&__z);
            our %EXPORT_GEN  = (
                'gena' => sub {
                    sub { 'a' }
                },
                '&genb' => sub {
                    sub { 'b' }
                },
            );

            sub foo { 'foo' }
            sub bar { 'bar' }
            sub baz { 'baz' }
            sub ick { 'ick' }
            sub __x { 'x' }
            sub __z { 'z' }

            # These are here to insure 'exports' does not pull them in, they are listed as generate
            sub gena { sub { 'oops, should not see this' } }
            sub genb { sub { 'oops, should not see this' } }

            # These are here to insure 'exports' does not pull them in, refs were provided by anon
            sub x { sub { 'oops, should not see this' } }
            sub y { sub { 'oops, should not see this' } }
        }

        my $one = $CLASS->new(from => 'Fake::ExporterE', caller => ['fake', 'fake.pl', 42]);
        my $menu = $one->reload_menu('fake');
        is($one->{menu_for}, 'fake', "remember who it was generated for");
        ok(my $gen = delete $menu->{generate}, "got a generate function");

        is_deeply(
            $menu,
            {
                lookup => {qw/
                    foo 1       &foo 1
                    bar 1       &bar 1
                    baz 1       &baz 1
                    ick 1       &ick 1
                    missing 1   &missing 1
                    x 1         &x 1
                    z 1         &z 1
                    gena 1      &gena 1
                    genb 1      &genb 1

                    $ZAP 1 %ZAP 1 @ZAP 1
                /},
                exports => {
                    '&foo' => Fake::ExporterE->can('foo'),
                    '&bar' => Fake::ExporterE->can('bar'),
                    '&baz' => Fake::ExporterE->can('baz'),
                    '&ick' => Fake::ExporterE->can('ick'),
                    '&x' => Fake::ExporterE->can('__x'),
                    '&z' => Fake::ExporterE->can('__z'),

                    '&missing' => undef,

                    '$ZAP' => \$Fake::ExporterE::ZAP,
                    '@ZAP' => \@Fake::ExporterE::ZAP,
                    '%ZAP' => \%Fake::ExporterE::ZAP,
                },
                tags => {
                    b => [qw/bar baz/],
                    DEFAULT => [qw/foo &bar $ZAP %ZAP @ZAP/],
                },
                fail => { '&ick' => 1, ick => 1 },
            },
            "Got menu"
        );

        is($gen->('gena')->(), 'a', "generated a");
        is($gen->('genb')->(), 'b', "generated b");
    };

};

done_testing;

__END__


sub parse_args {
    my $self = shift;
    my ($into, @args) = @_;

    @args = (':DEFAULT') unless @args;

    my $from = $self->from;
    my $menu = $self->menu($into);

    my %exclude;
    my @import;
    my @versions;

    while(my $full_arg = shift @args) {
        my $arg = $full_arg;
        my $lead = substr($arg, 0, 1);
        my ($spec, $exc);

        # If the first character is an ASCII numeric then it is a version number
        if ($NUMERIC{$lead}) {
            push @versions => $arg;
            next;
        }

        if ($lead eq '!') {
            my $exc = $lead;

            if ($arg eq '!') {
                # If the current arg is just '!' then we are negating the next item.
                $arg = shift;
            }
            else {
                # Strip off the '!'
                substr($arg, 0, 1, '');

                # Exporter.pm legacy behavior
                # negated first item implies starting with default set:
                unshift @args => ':DEFAULT' unless @import || keys %exclude || @versions;
            }

            # Now we have a new lead character
            $lead = substr($arg, 0, 1);
        }
        else {
            # If the item is followed by a reference then they are asking us to
            # do something special...
            $spec = ref($args[0]) ? shift @args : {};
        }

        # Process the item to figure out what symbols are being touched, if it
        # is a tag or regex than it can be multiple.
        my @list;
        if(ref($arg) eq 'Regexp') {
            @list = grep /$arg/, keys %{$menu->{lookup}};
        }
        if($lead eq ':') {
            substr($arg, 0, 1, '');
            my $tag = $menu->{tags}->{$arg} or croak "$from does not export the :$arg tag";
            @list = @$tag;
        }
        elsif($lead eq '/' && $arg =~ m{^/(.*)/$}) {
            my $pattern = $1;
            @list = grep /$1/, keys %{$menu->{lookup}};
        }
        else {
            @list = ($arg);
        }

        # Normalize list, always have a sigil
        @list = map {m/^\W/ ? $_ : "\&$_" } @list;

        if ($exc) {
            $exclude{$_} = 1 for @list;
        }
        else {
            $self->croak("Cannot use '-as' to rename multiple symbols included by: $full_arg")
                if $spec->{'-as'} && @list > 1;

            push @import => [$_, $spec] for @list;
        }
    }

    return ($into, \@versions, \%exclude, \@import);
}

sub _handle_fail {
    my $self = shift;
    my ($into, $import) = @_;

    my $from = $self->from;
    my $menu = $self->menu($into);

    my @fail = grep { $menu->{fail}->{$_->[0]} } @$import or return;

    my @real_fail = $from->export_fail(map {$_->[0]} @fail) if $from->can('export_fail');

    if (@real_fail) {
        $self->carp(qq["$_" is not implemented by the $from module on this architecture])
            for @real_fail;

        $self->croak("Can't continue after import errors");
    }

    $self->reload_menu($menu);
    return;
}

sub _set_symbols {
    my $self = shift;
    my ($into, $exclude, $import) = @_;

    my $from   = $self->from;
    my $menu   = $self->menu($into);
    my $caller = $self->get_caller();

    my $set_symbol = eval <<"    EOT" || die $@;
#line ${ \__LINE__ } "${ \__FILE__ }"
        sub {
            my (\$name, \$ref) = \@_;

            # Inherit the callers warning settings. If they have warnings and we
            # redefine their subs they will hear about it. If they do not have warnings
            # on they will not.
            BEGIN { \${^WARNING_BITS} = \$caller->[9] if \$caller->[9] };

            # For our sub here we want to keep most strictures on, but we need to turn
            # off strict ref checking.
            no strict 'refs';

#line $caller->[2] "$caller->[1]"
            *{"$into\::\$name"} = \$ref;
        }
    EOT

    for my $set (@$import) {
        my ($symbol, $spec) = @$set;

        my ($sig, $name) = ($symbol =~ m/^(\W)(.*)$/);

        # Find the thing we are actually shoving in a new namespace
        my $ref = $menu->{exports}->{$symbol};
        $ref ||= $menu->{generate}->($symbol) if $menu->{generate};

        # Exporter.pm supported listing items in @EXPORT that are not actually
        # available for export. So if it is listed (lookup) but nothing is
        # there (!$ref) we simply skip it.
        croak "$from does not export $symbol" unless $ref || $menu->{lookup}->{$name} || $menu->{lookup}->{$symbol};
        next unless $ref;

        # Figure out the name they actually want it under
        $name = $spec->{'-as'} || join '' => ($spec->{'-prefix'} || '', $name, $spec->{'-postfix'} || '');

        # Skip it if it has been excluded. We check only the new name, if they
        # exclude an old name, and then ask for it with a new name we assume it
        # is just a rename with precautions.
        next if $exclude->{"${sig}${name}"};

        push @{$IMPORTED{$into}} => $name if $sig eq '&';

        # Set the symbol (finally!)
        $set_symbol->($name, $ref);
    }
}

#########################################################
## The rest of these are utility functions, not methods!

sub _version_check {
    my ($mod, $caller, @versions) = @_;

    eval <<"    EOT" or die $@;
#line $caller->[2] "$caller->[1]"
\$mod->VERSION(\$_) for \@versions;
1;
    EOT
}

sub _mod_to_file {
    my $file = shift;
    $file =~ s{::}{/}g;
    $file .= '.pm';
    return $file;
}

sub _load_file {
    my ($caller, $file) = @_;

    eval <<"    EOT" || die $@;
#line $caller->[2] "$caller->[1]"
require \$file;
    EOT
}

sub _optimal_import {
    my ($from, $into, @args) = @_;

    my %final;
    no strict 'refs';
    return 0 if @{"$from\::EXPORT_FAIL"};
    @args = @{"$from\::EXPORT"} unless @args;
    my %allowed = map +($_ => 1), @{"$from\::EXPORT"}, @{"$from\::EXPORT_OK"};
    use strict 'refs';

    for my $arg (@args) {
        # Get sigil, or first letter of name
        my $sig = substr($arg, 0, 1);

        # Return if non-sub sigil
        return 0 if $NON_OPTIMAL{$sig};

        # Strip sigil (if sub)
        my $name = $arg;
        substr($name, 0, 1, '') if $sig eq '&';

        # Check if the name is allowed (with or without sigil)
        return 0 unless $allowed{$name} || $allowed{$arg};

        no strict 'refs';
        $final{$name} = \&{"$from\::$name"};
    }

    no strict 'refs';
    (*{"$into\::$_"} = $final{$_}, push @{$IMPORTED{$into}} => $_) for keys %final;

    return 1;
}

sub exporter_import {
    my $from = shift;

    my @caller = caller(0);

    return unless @_;

    my $file = _mod_to_file($from);
    _load_file(\@caller, $file) unless $INC{$file};

    return if _optimal_import($from, $caller[0], @_);

    my $self = __PACKAGE__->new(
        from   => $from,
        caller => \@caller,
    );

    $self->do_import($caller[0], @_);
}


