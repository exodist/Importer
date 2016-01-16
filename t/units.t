use strict;
use warnings;

use Importer 'Test::More';

my $CLASS = 'Importer';

sub dies(&) {
    my $code = shift;

    local $@;
    eval { $code->(); 1 } and return undef;

    return $@ || 1;
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

done_testing;

__END__

sub menu {
    my $self = shift;
    my ($into) = @_;

    $self->croak("menu() requires the name of the destination package")
        unless $into;

    my $for = $self->{menu_for};
    delete $self->{menu} if $for && $for ne $into;
    return $self->{menu} || $self->reload_menu($into);
}

sub reload_menu {
    my $self = shift;
    my ($into) = @_;

    $self->croak("reload_menu() requires the name of the destination package")
        unless $into;

    my $from = $self->from;

    my ($export, $export_ok, $export_tags, $export_fail, $generate);
    if ($from->can('IMPORTER_MENU')) {
        # Hook, other exporter modules can define this method to be compatible with
        # Importer.pm

        my %got = $from->IMPORTER_MENU($into, $self->get_caller);
        $export      = $got{export}      || [];
        $export_ok   = $got{export_ok}   || [];
        $export_tags = $got{export_tags} || {};
        $export_fail = $got{export_fail} || [];
        $generate    = $got{generate};
    }
    else {
        no strict 'refs';
        no warnings 'once';
        $export      = \@{"$from\::EXPORT"};
        $export_ok   = \@{"$from\::EXPORT_OK"};
        $export_tags = \%{"$from\::EXPORT_TAGS"};
        $export_fail = \@{"$from\::EXPORT_FAIL"};
    }

    my $exports = { map {
        my ($sig, $name) = (m/^(\W?)(.*)$/);
        $sig ||= '&';
        my $slot = $SIG_TO_SLOT{$sig} || 'CODE';

        no strict 'refs';
        no warnings 'once';
        ("${sig}${name}" => $slot eq 'SCALAR' ? \${"$from\::$_"} : *{"$from\::$_"}{$slot});
    } @$export, @$export_ok };

    my $tags = {
        %$export_tags,
        'DEFAULT' => [ @$export ],
    };

    my $fail = @$export_fail ? {
        map {
            my ($sig, $name) = (m/^(\W?)(.*)$/);
            $sig ||= '&';
            ("${sig}${name}" => 1)
        } @$export_fail
    } : undef;

    my $lookup = { map { $_ => 1 } @$export, @$export_ok };

    $self->{menu_for} = $into;
    return $self->{menu} = {
        lookup   => $lookup,
        exports  => $exports,
        tags     => $tags,
        fail     => $fail,
        generate => $generate,
    };
}



sub parse_args {
    my $self = shift;
    my ($into, @args) = @_;

    @args = (':DEFAULT') unless @args;

    my $from = $self->from;
    my $menu = $self->menu($into);

    my %exclude;
    my @import;
    my @versions;

    while(my $arg = shift @args) {
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

        my ($sig, $name) = ($symbol =~ m/^(\W?)(.*)$/);

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

sub do_import {
    my $self = shift;

    my ($into, $versions, $exclude, $import) = $self->parse_args(@_);

    # Exporter supported multiple version numbers being listed...
    _version_check($self->from, $self->get_caller, @$versions) if @$versions;

    return unless @$import;

    $self->_handle_fail($into, $import) if $self->menu($into)->{fail};
    $self->_set_symbols($into, $exclude, $import);
}

sub do_unimport {
    my $self = shift;

    my $from = $self->from;
    my $imported = $IMPORTED{$from} || $self->croak("'$from' does not have any imports to remove");

    my %allowed = map { $_ => 1 } @$imported;

    my @args = @_ ? @_ : @$imported;

    no strict 'refs';
    my $stash = \%{"$from\::"};
    use strict 'refs';

    for my $name (@args) {
        $name =~ s/^&//;

        $self->croak("Sub '$name' was not imported using " . ref($self)) unless $allowed{$name};

        no warnings 'once';
        my $glob = delete $stash->{$name};
        local *GLOBCLONE = *$glob;

        for my $type (qw/SCALAR HASH ARRAY FORMAT IO/) {
            next unless defined(*{$glob}{$type});
            no strict 'refs';
            *{"$from\::$name"} = *{$glob}{$type}
        }
    }
}


1;

