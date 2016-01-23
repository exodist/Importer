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
            sub gena { die 'oops, should not see this' }
            sub genb { die 'oops, should not see this' }

            # These are here to insure 'exports' does not pull them in, refs were provided by anon
            sub x { die 'oops, should not see this' }
            sub y { die 'oops, should not see this' }

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
            sub gena { die 'oops, should not see this' }
            sub genb { die 'oops, should not see this' }

            # These are here to insure 'exports' does not pull them in, refs were provided by anon
            sub x { die 'oops, should not see this' }
            sub y { die 'oops, should not see this' }
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

subtest parse_args => sub {
    {
        package Fake::Exporter::ForArgs;

        sub IMPORTER_MENU {
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
        sub gena { die 'oops, should not see this' }
        sub genb { die 'oops, should not see this' }

        # These are here to insure 'exports' does not pull them in, refs were provided by anon
        sub x { die 'oops, should not see this' }
        sub y { die 'oops, should not see this' }
    }

    my $one = $CLASS->new(from => 'Fake::Exporter::ForArgs', caller => ['Foo', 'foo.pl', 42]);

    is_deeply(
        [$one->parse_args('Dest')],
        [
            'Dest',
            [],
            {},
            [
                ['&foo', {}],
                ['&bar', {}],
                ['$ZAP', {}],
                ['%ZAP', {}],
                ['@ZAP', {}],
            ]
        ],
        "Got defaults with empty list"
    );

    is_deeply(
        [$one->parse_args('Dest', '!bar')],
        [
            'Dest',
            [],
            { '&bar' => 1 },
            [
                ['&foo', {}],
                ['&bar', {}],
                ['$ZAP', {}],
                ['%ZAP', {}],
                ['@ZAP', {}],
            ]
        ],
        "Got defaults, exclude bar"
    );

    is_deeply(
        [$one->parse_args('Dest', '!' => 'bar')],
        [
            'Dest',
            [],
            { '&bar' => 1 },
            [
                ['&foo', {}],
                ['&bar', {}],
                ['$ZAP', {}],
                ['%ZAP', {}],
                ['@ZAP', {}],
            ]
        ],
        "Got defaults, exclude bar"
    );

    is_deeply(
        [$one->parse_args('Dest', ':DEFAULT', '!:b')],
        [
            'Dest',
            [],
            { '&bar' => 1, '&baz' => 1 },
            [
                ['&foo', {}],
                ['&bar', {}],
                ['$ZAP', {}],
                ['%ZAP', {}],
                ['@ZAP', {}],
            ]
        ],
        "Got defaults, exclude :b"
    );

    is_deeply(
        [$one->parse_args('Dest', ':b' => {-prefix => 'foo_'}, qw/x &y/)],
        [
            'Dest',
            [],
            {},
            [
                ['&bar', {-prefix => 'foo_'}],
                ['&baz', {-prefix => 'foo_'}],
                ['&x', {}],
                ['&y', {}],
            ]
        ],
        "Spec for tag"
    );

    is_deeply(
        [$one->parse_args('Dest', '/A/' => { -postfix => '_foo' }, '!$ZAP')],
        [
            'Dest',
            [],
            { '$ZAP' => 1 },
            [
                ['$ZAP', {-postfix => '_foo'}],
                ['%ZAP', {-postfix => '_foo'}],
                ['@ZAP', {-postfix => '_foo'}],
            ]
        ],
        "Spec for pattern"
    );

    is_deeply(
        [$one->parse_args('Dest', 22, qr/A/, { -postfix => '_foo' }, '!$ZAP', 45)],
        [
            'Dest',
            [ 22, 45 ],
            { '$ZAP' => 1 },
            [
                ['$ZAP', {-postfix => '_foo'}],
                ['%ZAP', {-postfix => '_foo'}],
                ['@ZAP', {-postfix => '_foo'}],
            ]
        ],
        "Spec for qr// (also test version)"
    );

    like(
        dies { $one->parse_args('Dest', '/A/' => { -as => 'foo' }) },
        qr{Cannot use '-as' to rename multiple symbols included by: /A/},
        "-as does not work with multiple imports"
    );

    like(
        dies { $one->parse_args('Dest', ':b' => { -as => 'foo' }) },
        qr{Cannot use '-as' to rename multiple symbols included by: :b},
        "-as does not work with multiple imports"
    );

    like(
        dies { $one->parse_args('Dest', ':bad') },
        qr{Fake::Exporter::ForArgs does not export the :bad tag},
        "-as does not work with multiple imports"
    );
};

subtest _handle_fail => sub {
    {
        package Fake::Exporter::ForFail;

        sub IMPORTER_MENU {
            return (
                export      => [qw/foo &bar $ZAP %ZAP @ZAP/],
                export_ok   => [qw/baz ick missing/],
                export_tags => {b => [qw/bar baz/]},
                export_fail => [qw/ick foo/],
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
        sub gena { die 'oops, should not see this' }
        sub genb { die 'oops, should not see this' }

        # These are here to insure 'exports' does not pull them in, refs were provided by anon
        sub x { die 'oops, should not see this' }
        sub y { die 'oops, should not see this' }

        sub export_fail {
            my $from = shift;
            return grep !/foo/, @_;
        }
    }

    my $one = $CLASS->new(from => 'Fake::Exporter::ForFail', caller => ['Foo', 'foo.pl', 42]);

    ok(!dies { $one->_handle_fail('dest', [['bar'], ['baz']]) }, "no failures") || diag $@;
    ok(!dies { $one->_handle_fail('dest', [['bar'], ['foo']]) }, "no failures, but 'foo' was on list") || diag $@;

    like(
        warns {
            like(
                dies { $one->_handle_fail('dest', [['bar'], ['ick']]) },
                qr/Can't continue after import errors/,
                "True failure"
            )
        },
        qr/"ick" is not implemented by the Fake::Exporter::ForFail module on this architecture/,
        "Got expected warning"
    );
};

subtest _set_symbols => sub {
    {
        package Fake::ForSetSymbols;
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
                my $bad = 'bad';
                return \$bad; # To test sigil mismatch
            },
        );

        our @ZAP = (qw/Z A P/);
        our $ZAP = 'ZAP';
        our %ZAP = (ZAP => 1);

        sub foo { 'foo' }
        sub bar { 'bar' }
        sub baz { 'baz' }
        sub ick { 'ick' }
        sub __x { 'x' }
        sub __z { 'z' }

        # These are here to insure 'exports' does not pull them in, they are listed as generate
        sub gena { die 'oops, should not see this' }
        sub genb { die 'oops, should not see this' }

        # These are here to insure 'exports' does not pull them in, refs were provided by anon
        sub x { die 'oops, should not see this' }
        sub y { die 'oops, should not see this' }
    }

    my $one = $CLASS->new(from => 'Fake::ForSetSymbols', caller => ['Foo', 'foo.pl', 42]);

    $one->_set_symbols(
        'Fake::Dest::A',
        {'&bar' => 1, '@ZAP' => 1},
        [
            # These first 2 should both be excluded
            ['&bar' => {}],
            ['&bar' => {-prefix => 'pre_', -postfix => '_post'}],

            # Replicate use of ':b', this one is not excluded though
            ['&baz' => {-prefix => 'pre_', -postfix => '_post'}],

            # Exclude
            ['@ZAP' => {}],

            # Should import, specific name requested, ignore exclude
            ['&bar' => {-as => 'boo'}],

            # Should work fine
            ['&foo'  => {}],
            ['&gena' => {}],
            ['&x'    => {}],
            ['$ZAP'  => {-prefix => 'pre_', -postfix => '_post'}],
        ],
    );

    is(\&Fake::Dest::A::pre_baz_post, \&Fake::ForSetSymbols::baz, 'Exported &baz as pre_baz_post');
    is(\&Fake::Dest::A::boo,          \&Fake::ForSetSymbols::bar, 'Exported &bar as &boo');
    is(\&Fake::Dest::A::foo,          \&Fake::ForSetSymbols::foo, 'Exported &foo');
    is(\&Fake::Dest::A::x,            \&Fake::ForSetSymbols::__x, 'Exported anon &x');
    is(\$Fake::Dest::A::pre_ZAP_post, \$Fake::ForSetSymbols::ZAP, 'Exported $ZAP as $pre_ZAP_post');
    is(Fake::Dest::A::gena(),         'a',                        'Generated &gena');

    {
        no warnings 'once';
        ok(\@Fake::Dest::A::ZAP != \@Fake::ForSetSymbols::ZAP,          'Excluded @ZAP');
        ok(\&Fake::Dest::A::bar != \&Fake::ForSetSymbols::bar,          'Excluded &bar');
        ok(\&Fake::Dest::A::pre_bar_post != \&Fake::ForSetSymbols::bar, 'Excluded &bar with prefix/postfix');
    }

    ok(!dies { $one->_set_symbols('Fake::Dest::A', {}, [['&missing' => {}]]) }, "Can fake-import missing symbol if it is listed");

    like(
        dies { $one->_set_symbols('Fake::Dest::A', {}, [['&nope' => {}]]) },
        qr/Fake::ForSetSymbols does not export \&nope/,
        "unlisted symbol cannot be imported"
    );

    like(
        dies { $one->_set_symbols('Fake::Dest::A', {}, [['&genb' => {}]]) },
        qr/Symbol '\&genb' requested, but reference \(SCALAR\) does not match sigil \(\&\)/,
        "sigil mismatch"
    );

    # Make sure it finds the correct caller, not our fake one
    delete $one->{caller};

    {
        no warnings 'redefine';
        *Fake::Dest::A::foo = sub { 1 };
    }

    ok(
        !warns {
            no warnings 'redefine';
            $one->_set_symbols('Fake::Dest::A', {}, [['&foo' => {}]])
        },
        "no redefine warnings"
    );

    {
        no warnings 'redefine';
        *Fake::Dest::A::foo = sub { 1 };
    }

    like(
        warns {
            use warnings 'redefine';
            $one->_set_symbols('Fake::Dest::A', {}, [['&foo' => {}]])
        },
        qr/Subroutine Fake::Dest::A::foo redefined/,
        "redefine warnings"
    );

    $one = $CLASS->new(from => 'Fake::Dest::A');

    can_ok('Fake::Dest::A', 'foo');
    $one->do_unimport(qw/foo/);
    ok(!'Fake::Dest::A'->can('foo'), "removed &foo");

    is(\&Fake::Dest::A::pre_baz_post, \&Fake::ForSetSymbols::baz, 'Kept &baz as pre_baz_post');
    is(\&Fake::Dest::A::boo,          \&Fake::ForSetSymbols::bar, 'Kept &bar as &boo');
    is(\&Fake::Dest::A::x,            \&Fake::ForSetSymbols::__x, 'Kept anon &x');
    is(\$Fake::Dest::A::pre_ZAP_post, \$Fake::ForSetSymbols::ZAP, 'Kept $ZAP as $pre_ZAP_post');
    is(Fake::Dest::A::gena(),         'a',                        'Kept &gena');

    $one->do_unimport();
    is(\$Fake::Dest::A::pre_ZAP_post, \$Fake::ForSetSymbols::ZAP, 'Kept $ZAP as $pre_ZAP_post');
    ok(!'Fake::Dest::A'->can($_), "removed \&$_") for qw/pre_baz_post boo x gena/;
};

subtest version_check => sub {
    local *version_check = $CLASS->can('_version_check') or die "where did _version_check go?";
    ok(version_check($CLASS, ['foo', 'foo.pl', 42], '0.001'), "version check pass");
    like(
        dies { version_check($CLASS, ['foo', 'foo.pl', 42], '9999') },
        qr/version 9999 required.*foo\.pl line 42/,
        "Version Check fails"
    );
};

subtest mod_to_file => sub {
    local *mod_to_file = $CLASS->can('_mod_to_file') or die "where did _mod_to_file go?";
    is(mod_to_file('Foo::Bar::Baz'), 'Foo/Bar/Baz.pm', "Converted module to filename");
};

subtest load_file => sub {
    local *load_file = $CLASS->can('_load_file') or die "where did _load_file go?";
    ok(load_file(['foo', 'foo.pl', 42], 'Data/Dumper.pm'), "Load file pass");
    eval <<"    EOT" && die "Ooops, wtf?";
#line 42 "foo.pl"
require Fake::File::That::Better::Not::Exist::SAGSDGDS;
1;
    EOT
    my $error = $@;
    like($error, qr/locate.*\@INC/ms, "predicted error message is somewhat sane");
    is(
        dies { load_file(['foo', 'foo.pl', 42], 'Fake/File/That/Better/Not/Exist/SAGSDGDS.pm') },
        $error,
        "Load file fails"
    );
};

subtest _optimal_import => sub {
    {
        package Fake::ForOptimal::A;
        our @EXPORT = qw/foo &bar $ZAP %ZAP @ZAP/;
        sub foo { 'foo' }
        sub bar { 'bar' }
    }
    my $optimal = $CLASS->can('_optimal_import');

    ok($optimal->('Fake::ForOptimal::A', 'FDestA', ['F', 'F.pm', 4], qw/foo/), "Success");
    can_ok('FDestA', 'foo');

    ok(!$optimal->('Fake::ForOptimal::A', 'FDestA', ['F', 'F.pm', 4], qw/bar @ZAP/), "Failure");
    ok(!'FDestA'->can('bar'), 'Did not export anything');

    ok(!$optimal->('Fake::ForOptimal::A', 'FDestA', ['F', 'F.pm', 4], qw/bloop/), "Failure, not a valid export");

    {
        package Fake::ForOptimal::B;
        our @EXPORT = qw/foo &bar/;
        sub foo { 'foo' }
        sub bar { 'bar' }
    }
    ok($optimal->('Fake::ForOptimal::B', 'FDestB', ['F', 'F.pm', 4]), "Success with defaults");
    can_ok('FDestB', 'foo', 'bar');

    {
        package Fake::ForOptimal::C;
        our @EXPORT = qw/foo &bar/;
        our @EXPORT_FAIL = qw/bar/;
        sub foo { 'foo' }
        sub bar { 'bar' }
    }
    ok(!$optimal->('Fake::ForOptimal::C', 'FDestC', ['F', 'F.pm', 4], 'foo'), "Failure die to EXPORT_FAIL");
    ok(!'FDestC'->can('foo'), 'Did not export anything');



    no warnings 'once';
    *FDestD::foo = sub { 'xyz' };
    like(
        warns { $optimal->('Fake::ForOptimal::A', 'FDestD', ['F', 'F.pm', 4], 'foo') },
        qr/Subroutine FDestD::foo redefined at F\.pm line 4/,
        "Got redefine warning"
    );

    {
        package FDestD;
        Importer->unimport;
    }

    ok(!FDestD->can('foo'), "Removed 'foo'");
};

done_testing;
