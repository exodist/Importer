package Importer::Exporter;
use strict;
use warnings;

use Importer Importer => (
    exporter_import => { -as => 'import' },
    exporter_import => { -as => 'export' },
);

our @EXPORT_OK = qw/import/;

sub export_fail { shift; @_ }

sub export_to_level {
    my $from = shift;
    my ($level, $ignore, @args) = @_;
    Importer->import_into($from, $level + 1, @args);
}

sub require_version {
    my ($self, $wanted) = @_;
    my $pkg = ref $self || $self;
    return ${pkg}->VERSION($wanted);
}

my $push_tags = sub {
    my $from = shift;
    my ($var, @tags) = @_;

    no strict 'refs';
    my $export      = \%{"$from\::$var"};
    my $export_tags = \%{"$from\::EXPORT_TAGS"};
    use strict 'refs';

    my @nontag = ();
    for my $tag (@tags ? @tags : keys %$export_tags) {
        my $tag_list = $export_tags->{$tag};
        $tag_list ? push @$export => @$tag_list : push @nontag => $tag;
    };

    return unless @nontag && $^W;

    require Carp;
    Carp::carp(join(", ", @nontag) . " are not tags of $from");
};

sub export_tags {
    my $from = shift;
    $from->$push_tags('EXPORT', @_);
}

sub export_ok_tags {
    my $from = shift;
    $from->$push_tags('EXPORT_OK', @_);
}

1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Importer::Exporter - DO NOT USE THIS

=head1 DESCRIPTION

This is what L<Exporter> would look like if it used L<Importer> to get the job
done. Works with C<use base 'Importer::Exporter';> as well as
C<use Importer::Exporter qw/import/>.

=head1 *** EXPERIMENTAL ***

This module is still experimental. Anything can change at any time. Testing is
currently VERY insufficient.

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
