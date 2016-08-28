use strict;
use warnings;

use Importer 'Test::More';

our %USED;

BEGIN {
    $INC{'My/Exporter.pm'} = 1;
    package My::Exporter;

    sub x { 'x' }
    sub y { 'y' }
    sub z { 'z' }

    my $on_use = sub {
        return unless $main::ON_USE;
        $main::USED{$_[0] || '<NO PIN SPECIFIED>'}++;
    };

    sub IMPORTER_MENU {
        return (
            export_anon => {
                a => sub { 'a0' },
                b => sub { 'b0' },
                c => sub { 'c0' },
                d => sub { 'd0' },
                e => sub { 'e0' },
            },

            export_pins => {
                inherit => 'base',

                base => {
                    export => [qw/x/],
                    export_ok => [qw/y z/],
                    export_on_use => $on_use,
                    generate => sub {
                        my $symbol = shift;
                        my ($sig, $name) = ($symbol =~ m/^(\W?)(.*)$/);
                        $sig ||= '&';

                        return undef unless $sig eq '&';
                        return undef unless $name eq 'g1';

                        return sub { 'g1' };
                    },
                },
                v1 => {
                    inherit => 'base',
                    export => [qw/a b c/],
                    export_anon => {
                        a => sub { 'a1' },
                        b => sub { 'b1' },
                        c => sub { 'c1' },
                    },
                    generate => sub {
                        my $symbol = shift;
                        my ($sig, $name) = ($symbol =~ m/^(\W?)(.*)$/);
                        $sig ||= '&';

                        return undef unless $sig eq '&';
                        return undef unless $name eq 'g2';

                        return sub { 'g2' };
                    },
                },
                v2 => {
                    inherit => 'base',
                    export => [qw/a b c/],
                    export_on_use => sub { $on_use->(@_), $main::USED{v2_2} = 'yes' },
                    export_anon => {
                        a => sub { 'a2' },
                        b => sub { 'b2' },
                        c => sub { 'c2' },
                    },
                },
            },
        );
    }

    $INC{'My/Exporter2.pm'} = 1;
    package My::Exporter2;

    sub IMPORTER_MENU {
        return (
            export_anon => {
                foo => sub { 'foo' },
            },
            export_pins => {
                root_name => 'my_root_ver',
            },
        );
    }
}

{
    package My::Importer::A;
    use Importer 'My::Exporter' => qw/a b c d e/;
    ::is(a(), 'a0', "got versionless a()");
    ::is(b(), 'b0', "got versionless b()");
    ::is(c(), 'c0', "got versionless c()");
    ::is(d(), 'd0', "got versionless d()");
    ::is(e(), 'e0', "got versionless d()");
}

{
    package My::Importer::B;
    use Importer 'My::Exporter' => qw/d +v1 a b +v2 c +v0 e/;

    ::is(a(), 'a1', "got v1 a()");
    ::is(b(), 'b1', "got v1 b()");

    ::is(c(), 'c2', "got v2 c()");

    ::is(d(), 'd0', "got versionless d()");

    ::is(e(), 'e0', "got versionless e()");
}

{
    package My::Importer::C;
    use Importer 'My::Exporter' => qw/:v1 +v1 g1 g2/;

    ::is(a(), 'a1', "got v1 a()");
    ::is(b(), 'b1', "got v1 b()");
    ::is(c(), 'c1', "got v1 c()");
    ::is(x(), 'x',  "got common x()");

    ::is(g1(), 'g1', "Used common generate");
    ::is(g2(), 'g2', "Used version generate");

    ::ok(!__PACKAGE__->can('d'), "Did not import d()");
}

{
    package My::Importer::D;
    Importer->import('My::Exporter2', '+my_root_ver', 'foo');
    ::can_ok(__PACKAGE__, 'foo');
}

$main::ON_USE = 1;
{
    package My::On::Use::A;
    Importer->import('My::Exporter');
    ::can_ok(__PACKAGE__, 'x');
    ::is($main::USED{'<NO PIN SPECIFIED>'}, 1, "noted that we used v0");

    package My::On::Use::B;
    Importer->import('My::Exporter');
    ::is($main::USED{'<NO PIN SPECIFIED>'}, 2, "noted that we used v0 again");

    package My::On::Use::C;
    Importer->import('My::Exporter', ':v1');
    ::is($main::USED{v1}, 1, "noted that we used v1");

    package My::On::Use::D;
    Importer->import('My::Exporter', ':v2');
    ::is($main::USED{v2}, 1, "noted that we used v2");
    ::is($main::USED{v2_2}, 'yes', "Used the v2 specific on_use sub");
}

done_testing;
