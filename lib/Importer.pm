package Importer;
use strict;
use warnings;

our $VERSION = 0.001;

my %SIG_TO_SLOT = (
    '&' => 'CODE',
    '$' => 'SCALAR',
    '%' => 'HASH',
    '@' => 'ARRAY',
    '*' => 'GLOB',
);

my %IMPORTED;

# This will be used to check if an import arg is a version number
my %NUMERIC = map { $_ => 1 } 0 .. 9;

sub _version_check {
    my ($mod, $caller, @versions) = @_;

    eval <<"    EOT" or die $@;
#line $caller->[2] "$caller->[1]"
\$mod->VERSION(\$_) for \@versions;
1;
    EOT
}

sub import {
    my $class = shift;

    my @caller = caller(0);

    _version_check($class, \@caller, shift @_) if @_ && $NUMERIC{substr($_[0], 0, 1)};

    return unless @_;

    my ($from, @args) = @_;

    my $self = $class->new(
        from   => $from,
        caller => \@caller,
    );

    $self->load_from() unless $INC{$self->from_file()};
    $self->do_import($caller[0], @args);
}

sub import_into {
    my $class = shift;
    my ($from, $into, @args) = @_;

    my @caller;

    if ($into =~ m/^\d+$/) {
        @caller = caller($into + 1);
        $into = $caller[0];
    }
    else {
        @caller = caller(0);
    }

    my $self = $class->new(
        from   => $from,
        caller => \@caller,
    );

    $self->load_from() unless $INC{$self->from_file()};
    $self->do_import($caller[0], @args);
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

sub unimport {
    my $class = shift;
    my @caller = caller(0);

    my $self = $class->new(
        from   => $caller[0],
        caller => \@caller,
    );

    $self->do_unimport(@_);
}

sub unimport_from {
    my $class = shift;
    my ($from, @args) = @_;

    my @caller;
    if ($from =~ m/^\d+$/) {
        @caller = caller($from + 1);
        $from = $caller[0];
    }
    else {
        @caller = caller(0);
    }

    my $self = $class->new(
        from   => $from,
        caller => \@caller,
    );

    $self->do_unimport(@args);
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

sub new {
    my $class = shift;
    my %params = @_;

    my $caller = $params{caller} || [caller()];

    die "You must specify a package to import from at $caller->[1] line $caller->[2].\n"
        unless $params{from};

    return bless {
        from   => $params{from},
        caller => $params{caller},    # Do not use our caller.
    }, $class;
}

sub from { $_[0]->{from} }

sub from_file {
    my $self = shift;

    unless($self->{from_file}) {
        my $file = $self->{from};
        $file =~ s{::}{/}g;
        $file .= '.pm';
        return $self->{from_file} = $file;
    }

    return $self->{from_file};
}

sub load_from {
    my $self = shift;
    my $from_file = $self->from_file;
    my $this_file = __FILE__;

    return if $INC{$from_file};

    my $caller = $self->get_caller;

    eval <<"    EOT" || die $@;
#line $caller->[2] "$caller->[1]"
require \$from_file;
    EOT
}

sub get_caller {
    my $self = shift;
    return $self->{caller} if $self->{caller};

    my $level = 1;
    while(my @caller = caller($level)) {
        return \@caller if @caller && !$caller[0]->isa(__PACKAGE__);
    }

    # Fallback
    return [caller(1)];
}

sub croak {
    my $self = shift;
    my ($msg) = @_;
    my $caller = $self->get_caller;
    die "$msg at $caller->[1] line $caller->[2].\n";
}

sub carp {
    my $self = shift;
    my ($msg) = @_;
    my $caller = $self->get_caller;
    warn "$msg at $caller->[1] line $caller->[2].\n";
}

sub menu {
    my $self = shift;
    my ($into) = @_;
    my $for = $self->{menu_for};
    delete $self->{menu} if $for && $for ne $into;
    return $self->{menu} || $self->reload_menu($into);
}

sub reload_menu {
    my $self = shift;
    my ($into) = @_;
    my $from = $self->from;

    # Hook, other exporter modules can define this method to be compatible with
    # Importer.pm

    my ($export, $export_ok, $export_tags, $export_fail, $generate);
    if ($from->can('IMPORTER_MENU')) {
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
        my $exc  = $lead eq '!' ? 1 : 0;
        my $spec;

        # If the first character is an ASCII numeric then it is a version number
        if ($NUMERIC{$lead}) {
            push @versions => $arg;
            next;
        }

        if ($exc) {
            if ($arg eq '!') {
                # If the current arg is just '!' then we are negating the next item.
                $arg = shift;
            }
            else {
                # Strip off the '!'
                substr($arg, 0, 1, '');
            }

            # Now we have a new lead character
            $lead = substr($arg, 0, 1);

            # Exporter.pm legacy behavior
            # negated first item implies starting with default set:
            unshift @args => ':DEFAULT' unless @import || keys %exclude;
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

1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Importer - Alternative but compatible interface to modules that export symbols.

=head1 DESCRIPTION

This module acts as a layer between L<Exporter> and modules which consume
exports. It is feature-compatible with L<Exporter>, plus some much needed
extras. You can use this to import symbols from any exporter that follows
L<Exporters> specification. The exporter modules themselves do not need to use
or inherit from the L<Exporter> module, they just need to set C<@EXPORT> and/or
other variables.

=head1 *** EXPERIMENTAL ***

This module is still experimental. Anything can change at any time. Testing is
currently VERY insufficient.

=head1 SYNOPSYS

    # Import defaults
    use Importer 'Some::Module';

    # Import a list
    use Importer 'Another::Module' => qw/foo bar baz/;

    # Import a specific version:
    use Importer 'That::Module' => '1.00';

    # Require a sepcific version of Importer
    use Importer 0.001, 'Foo::Bar' => qw/a b c/;

    foo()
    bar()
    baz()

    # Remove all subroutines imported by Importer
    no Importer;

=head1 WHY?

There was recently a discussion on p5p about adding features to L<Exporter>.
This conversation raised some significant concerns, those are listed here, in
addition to others.

=over 4

=item The burden is on export consumers to specify a version of Exporter

Adding a feature to L<Exporter> means that any consumer module that relies on
the new features must depend on a specific version of L<Exporter>. This seems
somewhat backwords since L<Exporter> is used by the module you are importing
from.

=item Exporter.pm is really old/crazy code

Not much more to say here. It is very old, it is very crazy, and if you break
it you break EVERYTHING.

=item Using a modules import() for exporting makes it hard to give it other purposes

It is not unusual for a module to want to export symbols and prvide import
behaviors. It is also not unusual for a consumer to only want 1 or the other.
Using this module you can import symbols without also getting the C<import()>
side effects.

In addition, moving forward, modules can specify exports and have a custom
C<import()> without conflating the two. A module can tell you to use Importer
to get the symbols, and to use the module directly for behaviors. A module
could also use Importer within its own C<import()> method without the need to
subclass L<Exporter>, or bring in its C<import()> method.

=item There are other exporter modules on cpan

This module normally assumes an Exporter uses L<Exporter>, so it looks for the
variables and method L<Exporter> expects. However, other exporters on cpan can
override this using the C<IMPORTER_MENU()> hook.

=back

=head1 COMPATABILITY

This module aims for 100% compatabilty with every feature of L<Exporter>, plus
added features such as import renaming.

If you find something that works differently, or not at all when compared to
L<Exporter> please report it as a bug, unless it is noted as an intentional
feature (like import renaming).

=head1 IMPORT PARAMETERS

    use Importer $IMPORTER_VERSION, $FROM_MODULE, $FROM_MODULE_VERSION, @SYMBOLS;

=over 4

=item $IMPORTER_VERSION (optional)

If you provide a numeric argument as the first argument it will be treated as a
version number. Importer will d a version check to make sure it is at least at
the requested version.

=item $FROM_MODULE (required)

This is the only required argument. This is the name of the module to import
symbols from.

=item $FROM_MODULE_VERSION (optional)

Any numeric argument following the C<$FROM_MODULE> will be treated as a version
check against C<$FROM_MODULE>.

=item @SYMBOLS (optional)

Symbols you wish to import. If no symbols are specified then the defaults will
be used.

=back

=head1 SUPPORTED FEATURES

=head2 RENAMING SYMBOLS AT IMPORT

I<This is a new feature,> L<Exporter> I<does not support this on its own.>

You can rename symbols at import time using a specification hash following the
import name:

    use Importer 'Some::Thing' => (
        foo => { -as => 'my_foo' },
    );

You can also add a prefix and/or postfix:

    use Importer 'Some::Thing' => (
        foo => { -prefix => 'my_foo' },
    );

Using this syntax to set prefix and/or postfix also works on tags and patterns
that are specified for import, in which case the prefix/postfix is applied to
all symbols from the tag/patterm.

=head2 @EXPORT_FAIL

Use this to list subs that are not available on all platforms. If someone tries
to import one of these, Importer will hit your C< $from->export_fail(@items) >
callback to try to resolve the issue. See L<Exporter.pm> for documentation of
this feature.

=head2 %EXPORT_TAGS

This module supports tags exactly the way L<Exporter> does.

    use Importer 'Some::Thing'  => ':DEFAULT';

    use Importer 'Other::Thing' => ':some_tag';

=head2 /PATTERN/ or qr/PATTERN/

You can import all symbols that match a pattern. The pattern can be supplied a
string starting and ending with '/', or you can provide a C<qr/../> reference.

    use Importer 'Some::Thing' => '/oo/';

    use Importer 'Some::Thing' => qr/oo/;

=head2 EXLUDING SYMBOLS

You can exlude symbols by prefixing them with '!'.

    use Importer 'Some::Thing'
        '!foo',         # Exclude one specific symbol
        '!/pattern/',   # Exclude all matching symbols
        '!' => qr/oo/,  # Exclude all that match the following arg
        '!:tag';        # Exclude all in tag

=head1 UNIMPORT PARAMETERS

    no Importer;    # Remove all sub brought in with Importer

    no Importer qw/foo bar/;    # Remove only the specified subs

B<Only subs can be unimported>.

B<You can only unimport subs imported using Importer>.

=head1 CLASS METHODS

=over 4

=item Importer->import($from)

=item Importer->import($from, $version)

=item Importer->import($from, @imports)

=item Importer->import($from, $from_version, @imports)

=item Importer->import($importer_version, $from, ...)

This is the magic behind C<use Importer ...>.

=item Importer->import_into($from, $into, @imports)

=item Importer->import_into($from, $level, @imports)

You can use this to import symbols from C<$from> into C<$into>. C<$into> may
either be a package name, or a caller level to get the name from.

=item Importer->unimport()

=item Importer->unimport(@sub_name)

This is the magic behind C<no Importer ...>.

=item Importer->unimport_from($from, @sub_names)

=item Importer->unimport_from($level, @sub_names)

This lets you remove imported symbols from C<$from>. C<$from> my be a package
name, or a caller level.

=back

=head1 USING WITH OTHER EXPORTER IMPLEMENTATIONS

If you want your module to work with Importer, but you use something other than
L<Exporter> to define your exports, you can make it work be defining the
C<IMPORTER_MENU> method in your package. As well other exporters can be updated
to support Importer by putting this sub in your package:

    sub IMPORTER_MENU {
        my $class = shift;
        my ($into, $caller) = @_;

        return (
            export      => \@EXPORT,      # Default exports
            export_ok   => \@EXPORT_OK,   # Other allowed exports
            export_tags => \%EXPORT_TAGS, # Define tags
            export_fail => \@EXPORT_FAIL, # For subs that may not always be available
            generate    => \&GENERATE,    # Sub to generate dynamic exports
        );
    }

    sub GENERATE {
        my $class = shift;

        my ($symbol) = @_;

        ...

        return $ref;
    }

All exports must be listed in either C<@EXPORT> or C<@EXPORT_OK> to be allowed.
C<%EXPORT_TAGS>, C<@EXPORT_FAIL>, and C<\&GENERATE> are optional.

=head1 SOURCE

The source code repository for symbol can be found at
F<http://github.com/exodist/Importer>.

=head1 MAINTAINERS

=over 4

=item Chad Granum E<lt>exodist@cpan.orgE<gt>

=back

=head1 AUTHORS

=over 4

=item Chad Granum E<lt>exodist@cpan.orgE<gt>

=back

=head1 COPYRIGHT

Copyright 2015 Chad Granum E<lt>exodist7@gmail.comE<gt>.

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

See F<http://dev.perl.org/licenses/>

=cut
