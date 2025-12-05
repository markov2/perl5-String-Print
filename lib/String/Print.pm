#oodist: *** DO NOT USE THIS VERSION FOR PRODUCTION ***
#oodist: This file contains OODoc-style documentation which will get stripped
#oodist: during its release in the distribution.  You can use this file for
#oodist: testing, however the code of this development version may be broken!

package String::Print;

use warnings;
use strict;
use utf8;

#use Log::Report::Optional 'log-report';

use Unicode::GCString ();
use Data::Dumper      ();
use Date::Parse       qw/str2time/;
use Encode            qw/is_utf8 decode/;
use HTML::Entities    qw/encode_entities/;
use POSIX             qw/strftime/;
use Scalar::Util      qw/blessed reftype/;

my @default_modifiers   = (
	qr/\% ?\S+/      => \&_modif_format,
	qr/BYTES\b/      => \&_modif_bytes,
	qr/HTML\b/       => \&_modif_html,
	qr/YEAR\b/       => \&_modif_year,
	qr/DT\([^)]*\)/  => \&_modif_dt,
	qr/DT\b/         => \&_modif_dt,
	qr/DATE\b/       => \&_modif_date,
	qr/TIME\b/       => \&_modif_time,
	qr/\=/           => \&_modif_name,
	qr!UNKNOWN\([0-9]+\)|UNKNOWN\b!       => \&_modif_unknown,
	qr!CHOP\([0-9]+(?:\,?[^)]*)\)|CHOP\b! => \&_modif_chop,
	qr!EL\([0-9]+(?:\,?[^)]*)\)|EL\b!     => \&_modif_ellipsis,
	qr!//(?:\"[^"]*\"|\'[^']*\'|\w+)!     => \&_modif_undef,
);

my %defaults = (
	CHOP      => +{ width => 30, head => '[', units => '', tail => ']' },
	DT        => +{ standard => 'FT' },
	EL        => +{ width => 30, replace => '⋯ '},
	FORMAT    => +{ thousands => '' },
	UNKNOWN   => +{ width => 30, trim => 'EL' },
);

my %default_serializers = (
	UNDEF     => sub { 'undef' },
	''        => sub { $_[1]   },
	SCALAR    => sub { ${$_[1]} // shift->{SP_seri}{UNDEF}->(@_) },
	ARRAY     => sub { my $v = $_[1]; my $join = $_[2]{_join} // ', '; join $join, map +($_ // 'undef'), @$v },
	HASH      => sub { my $v = $_[1]; join ', ', map "$_ => ".($v->{$_} // 'undef'), sort keys %$v },
	# CODE value has different purpose
);

my %predefined_encodings = (
	HTML => {
		exclude => [ qr/html$/i ],
		encode  => sub { encode_entities $_[0] },
	},
);

=encoding utf8

=chapter NAME

String::Print - printf alternative

=chapter SYNOPSIS

  ### Functional interface

  use String::Print;           # simpelest way
  use String::Print qw/printi printp/, %config;

  printi 'age {years}', years => 12;   # to STDOUT
  my $s = sprinti 'age {years}', years => 12;  # in variable

  # interpolation of arrays and hashes (serializers)
  printi 'price-list: {prices}', prices => \@p, _join => "+";
  printi 'dump: {c}', c => \%data;

  # same with positional parameters
  printp 'age %d", 12;
  printp 'price-list: %.2f', \@prices;
  printp 'dump: %s', \%settings;
  my $s = sprintp 'age %d", 12;

  # modifiers
  printi 'price: {price%.2f}', price => 3.14 * EURO;

  # [0.91] more complex interpolation names
  printi 'filename: {c.filename}', c => \%data;
  printi 'username: {user.name}', user => $user_object;
  printi 'price: {product.price €}', product => $db->product(3);

  ### Object Oriented interface

  use String::Print 'oo', %config;      # import no functions
  my $f = String::Print->new(%config);
  $f->printi('age {years}', years => 12);
  $f->printp('age %d', 12);
  my $s = $f->sprinti('age {y}', y => 12);
  my $s = $f->sprintp('age %d', 12);

  ### via Log::Report's __* functions (optional translation)

  use Log::Report;             # or Log::Report::Optional
  print __x"age {years}", years => 12;

  ### via Log::Report::Template (Template Toolkit extension)

  [% SET name = 'John Doe' %]
  [% loc("Dear {name},") %]    # includes translation

=chapter DESCRIPTION

This module inserts values into (format) strings.  It provides C<printf()>
and C<sprintf()> alternatives via both an object oriented and a functional
interface.

Read in the L</DETAILS> chapter below, why this module provides a better
alternative for C<printf()>.  Also, some extended B<examples> can be
found down there.  Take a look at them first, when you start using this
module!

=chapter METHODS

See functions M<printi()>, M<sprinti()>, M<printp()>, and M<sprintp()>: you
can also call them as method.

  use String::Print 'oo';
  my $f = String::Print->new(%config);
  $f->printi($format, @params);

  # exactly the same functionality:
  use String::Print 'printi', %config;
  printi $format, @params;

The Object Oriented interface wins when you need the same configuration
in multiple source files, or when you need different configurations
within one program.  In these cases, the hassle of explicitly using the
object has some benefits.

=section Constructors

When you use this package with functions, then you do not call the
constructor explicitly.  In that case, these M<new()> parameters are
added to the C<<use String::Print>> line.

=c_method new %options
The %options of the constructure configure processing options.

=option  modifiers ARRAY
=default modifiers C<[ qr/^%\S+/ => \&format_printf]>
Add one or more modifier handlers to power of the formatter.  They will
get preference over the predefined modifiers, but lower than the modifiers
passed to C<print[ip]> itself.

=option  serializers HASH|ARRAY
=default serializers <useful defaults>
How to serialize data elements.

=option  encode_for \%rules|'HTML'
=default encode_for undef
[0.91] The format string and the inserted values will get encoded according to
some %rules.  Function C<encode_entities()> provided by HTML::Entities
is used when you specify the predefined string C<HTML>.  See M<encodeFor()>
and L</Output encoding>.

=option  missing_key CODE
=default missing_key <warning>
[0.91] During interpolation, it may be discovered that a key is missing
from the parameter list.  In that case, a warning is produced and undef
inserted.  May can overrule that behavior.

=option  defaults \%map
=default defaults C<see modifier docs>
[1.00] change the defaults for some modifiers.  This is a %map of
modifier name to HASH with modifier specific settings.

=examples

  my $f = String::Print->new(
    modifiers   => [ EUR   => sub {sprintf "%5.2f e", $_[0]} ],
    serializers => [ UNDEF => sub {'-'} ],
    encode_for  => 'HTML',
  );

  $f->printi("price: {p EUR}", p => 3.1415); # price: ␣␣3.14 e
  $f->printi("count: {c}", c => undef);      # count: -
=cut

sub new(@) { my $class = shift; (bless {}, $class)->init( +{@_} ) }

sub init($)
{	my ($self, $args) = @_;

	my $modif = $self->{SP_modif} = [ @default_modifiers ];
	if(my $m  = $args->{modifiers})
	{	unshift @$modif, @$m;
	}

	my $s    = $args->{serializers} || {};
	my $seri = $self->{SP_seri} = +{ %default_serializers, (ref $s eq 'ARRAY' ? @$s : %$s) };

	$self->{SP_defs} = +{ %defaults };  # the HASHes get copied when changed.
	$self->setDefaults($args->{defaults}) if $args->{defaults};

	$self->encodeFor($args->{encode_for});
	$self->{SP_missing} = $args->{missing_key} || \&_reportMissingKey;
	$self;
}

sub import(@)
{	my $class = shift;
	my ($oo, %func);
	while(@_)
	{	last if $_[0] !~ m/^s?print[ip]$/;
		$func{shift()} = 1;
	}

	if(@_ && $_[0] eq 'oo')
	{	# import only object oriented interface
		shift @_;
		@_ and die "no options allowed at import with oo interface";
		return;
	}

	my $all   = !keys %func;
	my $f     = $class->new(@_);   # OO encapsulated
	my ($pkg) = caller;
	no strict 'refs';
	*{"$pkg\::printi"}  = sub { $f->printi(@_)  } if $all || $func{printi};
	*{"$pkg\::sprinti"} = sub { $f->sprinti(@_) } if $all || $func{sprinti};
	*{"$pkg\::printp"}  = sub { $f->printp(@_)  } if $all || $func{printp};
	*{"$pkg\::sprintp"} = sub { $f->sprintp(@_) } if $all || $func{sprintp};
	$class;
}

#--------------------
=section Attributes

=method addModifiers PAIRS
The PAIRS are a combination of an selector and a CODE which processes the
value when the modifier matches.  The selector is a string or (preferred)
a regular expression. Later modifiers with the same name overrule earlier
definitions.  You may also specify an ARRAY of modifiers per M<printi()>
or M<printp()>.

See section L</"Interpolation: Modifiers"> about the details.
=cut

sub addModifiers(@) { my $s = shift; unshift @{$s->{SP_modif}}, @_ }

=method encodeFor \%settings|undef|($predefined, %overrule)
[0.91] Enable/define the output encoding.  The %settings contain the C<encoding>
CODE and the tag name patterns to C<exclude> from encoding.  Output for C<HTML>
is $predefined, but you may %overrule its settings.

Read section L</"Output encoding"> about the details.
=cut

sub encodeFor($)
{	my ($self, $type) = (shift, shift);
	defined $type
		or return $self->{SP_enc} = undef;

	my %def;
	if(ref $type eq 'HASH')
	{	%def = %$type;
	}
	else
	{	my $def = $predefined_encodings{$type} or die "ERROR: unknown output encoding type $type\n";
		%def = (%$def, @_);
	}

	my $excls   = $def{exclude} || [];
	my $regexes = join '|',
		map +(ref $_ eq 'Regexp' ? $_ : qr/(?:^|\.)\Q$_\E$/),
			ref $excls eq 'ARRAY' ? @$excls : $excls;
	$def{SP_exclude} = qr/$regexes/o;

	$self->{SP_enc} = \%def;
}

=method setDefaults \%defaults|@defaults
[1.00] Set the defaults for modifiers, either with a HASH where the key modifier name
maps to a HASH of settings, or a list of PAIRS.

When using the methods in OO style , you can change the defaults at any time.  For
functional style, the object is hidden.

=example setting defaults

  $sp->setDefaults(EL => {width => 30}, CHOP => {width => 10});

  my %defs = (EL => {width => 30}, CHOP => {width => 10});
  $sp->setDefaults(\%defs);

=cut

sub setDefaults(@)
{	my $self    = shift;
	my $default = $self->{SP_defs};

	my @set = @_==1 && ref $_[0] eq 'HASH' ? %{$_[0]} : @_;
	while(@set)
	{	my ($modif, $defs) = (shift @set, shift @set);
		my $was = $defaults{$modif} or die "No defaults available for $modif.";
		$default->{$modif} = +{ %$was, %$defs };
	}

	$self;
}

=method defaults $modifier
[1.00] Returns the current defaults for the $modifier.

=example getting defaults for a modifier
  my $def = $sp->defaults('EL');
  say "Default max width is ", $def->{width};
=cut

sub defaults($) { $_[0]->{SP_defs}{$_[1]} }

#--------------------
#XXX OODoc does not like it when we have methods and functions with the same name.

=section Printing

The following are provided both as method and as function.  The function
version is explained further down on this page, because it reads a bit
easier, but the object version is most flexible.

  my $sp = String::Print->new;
  $sp->printi([$fh], $format, %data|\%data);        # see M<printi()>
  $sp->printp([$fh], $format, @params, %options);   # see M<printp()>
  my $s = $sp->sprinti($format, %data|\%data);      # see M<sprinti()>
  my $s = $sp->sprintp($format, @params, %options); # see M<sprintp()>

=examples

  use String::Print 'oo';    # do not import the functions

  my $sp = String::Print->new(
    modifiers   => [ EUR   => sub {sprintf "%5.2f e", $_[0]} ],
    serializers => [ UNDEF => sub {'-'} ],
    defaults    => [ DT => { standard => 'ISO' } ],
  );

  $sp->printi("price: {p EUR}", p => 3.1415); # price: ␣␣3.14 e
  $sp->printi("count: {c}", c => undef);      # count: -
  my $s = $sp->sprinti("price: {p EUR}", p => 7); # output in $s

=cut

#--------------------
=chapter FUNCTIONS

The functional interface creates a hidden C<String::Print> object, which is
reused in the whole active package.  Seperate packages will use different
hidden objects.

You may import any of these functions explicitly, or all together by
not specifying the names.

=examples

  use String::Print;           # all
  use String::Print 'sprinti'; # only sprinti

  use String::Print 'printi',   # only printi
    modifiers   => [ EUR   => sub {sprintf "%5.2f e", $_[0]} ],
    serializers => [ UNDEF => sub {'-'} ],
    defaults    => [ DT => { standard => 'ISO' } ];

  printi "price: {p EUR}", p => 3.1415; # price: ␣␣3.14 e
  printi "count: {c}", c => undef;      # count: -
  my $s = sprinti "price: {p EUR}", p => 7;  # output in $s

=function sprinti $format, %data|\%data|OBJECT, %options
The $format refers to some string, maybe the result of a translation.

The %data (which may be passed as LIST, HASH, or blessed HASH) contains
a mixture of special and normal variables to be filled in.  The names
of the special variables (the %options) start with an underscore (C<_>).

=option  _count INTEGER
=default _count undef
Result of the translation process: when M<Log::Report::__xn()> is
are used for count-sensitive translation.  Those function may add
more specials to the parameter list.

=option  _join STRING
=default _join ', '
Which STRING to use when an ARRAY is being filled-in as parameter.

=option  _prepend STRING|OBJECT
=default _prepend undef
Text as STRING prepended before $format, without interpolation.  This
may also be an OBJECT which gets stringified, but variables not filled-in.

=option  _append  STRING|OBJECT
=default _append  undef
Text as STRING appended after $format, without interpolation.

=cut

sub sprinti($@)
{	my ($self, $format) = (shift, shift);
	my $args = @_==1 ? shift : +{ @_ };
	# $args may be a blessed HASH, for instance a Log::Report::Message

	$args->{_join} //= ', ';
	local $args->{_format} = $format;

	my @frags = split /\{([^}]*)\}/,   # enforce unicode
		is_utf8($format) ? $format : decode(latin1 => $format);

	my @parts;

	# Code parially duplicated for performance!
	if(my $enc = $self->{SP_enc})
	{	my $encode  = $enc->{encode};
		my $exclude = $enc->{SP_exclude};
		push @parts, $encode->($args->{_prepend}) if defined $args->{_prepend};
		push @parts, $encode->(shift @frags);
		while(@frags) {
			my ($name, $tricks) = (shift @frags) =~ m!^\s*([\pL\p{Pc}\pM][\w.]*)\s*(.*?)\s*$!o or die $format;

			push @parts, $name =~ $exclude
			  ? $self->_expand($name, $tricks, $args)
			  : $encode->($self->_expand($name, $tricks, $args));

			push @parts, $encode->(shift @frags) if @frags;
		}
		push @parts, $encode->($args->{_append}) if defined $args->{_append};
	}
	else
	{	push @parts, $args->{_prepend} if defined $args->{_prepend};
		push @parts, shift @frags;
		while(@frags) {
			(shift @frags) =~ /^\s*([\pL\p{Pc}\pM][\w.]*)\s*(.*?)\s*$/o or die $format;
			push @parts, $self->_expand($1, $2, $args);
			push @parts, shift @frags if @frags;
		}
		push @parts, $args->{_append} if defined $args->{_append};
	}

	join '', @parts;
}

sub _expand($$$)
{	my ($self, $key, $modifier, $args) = @_;
	local $args->{varname} = $key;

	my $value;
	if(index($key, '.') == -1)
	{	# simple value
		$value = exists $args->{$key} ? $args->{$key} : $self->_missingKey($key, $args);
		$value = $value->($self, $key, $args)
			while ref $value eq 'CODE';
	}
	else
	{	my @parts = split /\./, $key;
		my $key   = shift @parts;
		$value    = exists $args->{$key} ? $args->{$key} : $self->_missingKey($key, $args);

		$value = $value->($self, $key, $args)
			while ref $value eq 'CODE';

		while(defined $value && @parts)
		{	if(blessed $value)
			{	my $method = shift @parts;
				$value->can($method) or die "object $value cannot $method\n";
				$value = $value->$method;  # parameters not supported here
			}
			elsif(ref $value && reftype $value eq 'HASH')
			{	$value = $value->{shift @parts};
			}
			elsif(index($value, ':') != -1 || $::{$value.'::'})
			{	my $method = shift @parts;
				$value->can($method) or die "class $value cannot $method\n";
				$value = $value->$method;  # parameters not supported here
			}
			else
			{	die "not a HASH, object, or class at $parts[0] in $key\n";
			}

			$value = $value->($self, $key, $args)
				while ref $value eq 'CODE';
		}
	}

	my $mod;
  STACKED:
	while(length $modifier)
	{	my @modif = @{$self->{SP_modif}};
		while(@modif)
		{	my ($regex, $callback) = (shift @modif, shift @modif);
			$modifier =~ s/^($regex)\s*// or next;

			$value = $callback->($self, $1, $value, $args);
			next STACKED;
		}
		return "{unknown modifier '$modifier'}";
	}

	my $seri = $self->{SP_seri}{defined $value ? ref $value : 'UNDEF'};
	$seri ? $seri->($self, $value, $args) : "$value";
}

sub _missingKey($$)
{	my ($self, $key, $args) = @_;
	$self->{SP_missing}->($self, $key, $args);
}

sub _reportMissingKey($$)
{	my ($self, $key, $args) = @_;

	my $depth = 0;
	my ($filename, $linenr);
	while((my $pkg, $filename, $linenr) = caller $depth++)
	{	last unless $pkg->isa(__PACKAGE__) || $pkg->isa('Log::Report::Minimal::Domain');
	}

	warn $self->sprinti(
		"Missing key '{key}' in format '{format}', file {fn} line {line}\n",
		key => $key, format => $args->{_format}, fn => $filename, line => $linenr
	);

	undef;
}

# See dedicated section in explanation in DETAILS
sub _modif_format_s($$$$$)
{	my ($value, $padding, $width, $max, $u) = @_;

	# String formats like %10s or %-3.5s count characters, not width.
	# String formats like %10S or %-3.5S are subject to column width.
	# The latter means: minimal 3 chars, max 5, padding right with blanks.
	# All inserted strings are upgraded into utf8.

	my $s = Unicode::GCString->new(is_utf8($value) ? $value : decode(latin1 => $value));

	my $pad;
	if($u eq 'S')
	{	# too large to fit
		return $value if !$max && $width && $width <= $s->columns;

		# wider than max.  Waiting for $s->trim($max) if $max, see
		# https://rt.cpan.org/Public/Bug/Display.html?id=84549
		$s->substr(-1, 1, '')
			while $max && $s->columns > $max;

		$pad = $width ? $width - $s->columns : 0;
	}
	else  # $u eq 's'
	{	return $value if !$max && $width && $width <= length $s;
		$s->substr($max, length($s)-$max, '') if $max && length $s > $max;
		$pad = $width ? $width - length $s : 0;
	}

	  $pad==0         ? $s->as_string
	: $padding eq '-' ? $s->as_string . (' ' x $pad)
	:                   (' ' x $pad) . $s->as_string;
}

sub _modif_format_d($$$$)
{	my ($value, $padding, $max, $sep) = @_;
	my $d = sprintf "%d", $value;   # what perl usually does with floats etc
	my $v = length $sep ? reverse(reverse($d) =~ s/([0-9][0-9][0-9])/$1$sep/gr) : $d;
	$v =~ s/^\Q$sep//;

	if($d !~ /^\-/)
	{	$v = "+$v" if $padding eq '+';
		$v = " $v" if $padding eq ' ';
	}
	$max or return $v;

	my $pad = $max - length $v;

	    $pad <= 0       ? $v 
	  : $padding eq '-' ? $v . (' ' x $pad)
	  : $padding eq '0' ? ('0' x $pad) . $v
	  : $padding eq ''  ? (' ' x $pad) . $v
	  :   $v;
}

sub _modif_format($$$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value && length $value or return undef;

	my $defaults = $self->defaults('FORMAT');

	use locale;
	if(ref $value eq 'ARRAY')
	{	@$value or return '(none)';
		return +[ map $self->_format_print($format, $_, $args), @$value ];
	}
	elsif(ref $value eq 'HASH')
	{	keys %$value or return '(none)';
		return +{ map +($_ => $self->_format_print($format, $value->{$_}, $args)), keys %$value } ;
	}

	  $format =~ m/^\%(\-?)([0-9]*)(?:\.([0-9]*))?([sS])$/ ? _modif_format_s($value, $1, $2, $3, $4)
	: $format =~ m/^\%([+\ \-0]?)([0-9]*)([_,.])?d$/ ? _modif_format_d($value, $1, $2, $3 // $defaults->{thousands})
	:    return sprintf $format, $value;   # simple: standard perl sprintf()
}

# See dedicated section in explanation in DETAILS
sub _modif_bytes($$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value && length $value or return undef;

	return sprintf("%3d B", $value) if $value < 1000;

	my @scale = qw/kB MB GB TB PB EB ZB/;
	$value /= 1024;

	while(@scale > 1 && $value > 999)
	{	shift @scale;
		$value /= 1024;
	}

	return sprintf "%3d$scale[0]", $value + 0.5
		if $value > 9.949;

	sprintf "%3.1f$scale[0]", $value;
}

sub _modif_html($$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value ? (encode_entities $value) : undef;
}

# Be warned: %F and %T (from C99) are not supported on Windows
my %dt_format = (
	ASC     => '%a %b %e %T %Y',
	ISO     => '%Y-%m-%dT%T%z',
	RFC822  => '%a, %d %b %y %T %z',
	RFC2822 => '%a, %d %b %Y %T %z',
	RFC5322 => '%a, %d %b %Y %T %z',
	FT      => '%Y-%m-%d %T',
);

sub _modif_year($$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value or return undef;

	blessed $value && $value->isa('DateTime')
		and return $value->year;

	length $value or return undef;

	return $1
		if $value =~ /^\s*([0-9]{4})\s*$/ && $1 < 2200;

	my $stamp = $value =~ /^\s*([0-9]+)\s*$/ ? $1 : str2time($value);
	defined $stamp or return "year not found in '$value'";

	strftime "%Y", localtime($stamp);
}

sub _modif_date($$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value or return undef;

	blessed $value && $value->isa('DateTime')
		and return $value->ymd;

	length $value or return undef;

	return sprintf("%4d-%02d-%02d", $1, $2, $3)
		if $value =~ m!^\s*([0-9]{4})[:/.-]([0-9]?[0-9])[:/.-]([0-9]?[0-9])\s*$!
		|| $value =~ m!^\s*([0-9]{4})([0-9][0-9])([0-9][0-9])\s*$!;

	my $stamp = $value =~ /\D/ ? str2time($value) : $value;
	defined $stamp or return "date not found in '$value'";

	strftime "%Y-%m-%d", localtime($stamp);
}

sub _modif_time($$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value or return undef;

	blessed $value && $value->isa('DateTime')
		and return $value->hms;

	length $value or return undef;

	return sprintf "%02d:%02d:%02d", $1, $2, $3||0
		if $value =~ m!^\s*(0?[0-9]|1[0-9]|2[0-3])\:([0-5]?[0-9])(?:\:([0-5]?[0-9]))?\s*$!
		|| $value =~ m!^\s*(0[0-9]|1[0-9]|2[0-3])([0-5][0-9])(?:([0-5][0-9]))?\s*$!;

	my $stamp = $value =~ /\D/ ? str2time($value) : $value;
	defined $stamp or return "time not found in '$value'";

	strftime "%H:%M:%S", localtime($stamp);
}

sub _modif_dt($$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value or return undef;

	blessed $value && $value->isa('DateTime')
		and $value = $value->epoch;

	length $value or return undef;

	my $defaults = $self->defaults('DT');
	my $kind     = ($format =~ m/^DT\(([^)]*)\)/ ? $1 : undef) || $defaults->{standard};

	my $pattern  = $dt_format{$kind}
		or return "dt format $kind not known";

	my $stamp = $value =~ /\D/ ? str2time($value) : $value;
	defined $stamp or return "dt not found in '$value'";

	strftime $pattern, localtime($stamp);
}

sub _modif_undef($$$)
{	my ($self, $format, $value, $args) = @_;
	return $value if defined $value && length $value;
	$format =~ m!//"([^"]*)"|//'([^']*)'|//(\w*)! ? $+ : undef;
}

sub _modif_name($$$)
{	my ($self, $format, $value, $args) = @_;
	"$args->{varname}$format$value";
}

sub _modif_chop($$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value && length $value or return undef;

	my $defaults = $self->defaults('CHOP');
	$format =~ m/^ CHOP\( ([0-9]+) \,? ([^)]+)? \) | CHOP\b /x or die $format;
	my $width    = $1 // $args->{width} // $defaults->{width};
	my $units    = $2 // $args->{units} // $defaults->{units};
	$width != 0 or return $value;

	# max width of a char is 2
	return $value if 2 * length $value < $width;    # surely small enough?

	my $v = Unicode::GCString->new(is_utf8($value) ? $value : decode(latin1 => $value));
	return $value if $width >= $v->columns;          # small enough after counting

	my $head = $defaults->{head};
	my $tail = $defaults->{tail};

	#XXX This is expensive for long texts, but the value could be filled with many zero-widths
	my ($shortened, $append) = (0, $head . '+0' . $units . $tail);
	while($v->columns > $width - length $append)
	{	my $chopped = $v->substr(-1, 1, '');

		unless($chopped->length)
		{	# nothing left
			$append = $head . $shortened . $units . $tail;
			last;
		}

		$chopped->columns > 0 or next;
		$shortened++;
		$append     = $head . '+' . $shortened . $units . $tail;
	}

	# might be one column short
	my $pad = $v->columns < $width - (length $append) ? ' ' : '';
	$v->as_string . $pad . $append;
}

sub _modif_ellipsis($$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value && length $value or return undef;

	my $defaults = $self->defaults('EL');
	$format =~ m/^ EL\( ([0-9]+) \,? ([^)]+)? \) | EL\b /x or die $format;
	my $width    = $1 // $args->{width}   // $defaults->{width};
	my $replace  = $2 // $args->{replace} // $defaults->{replace};
	$width != 0 or return $value;

	# max width of a char is 2
	return $value if 2 * length($value) < $width;    # surely small enough?

	my $v = Unicode::GCString->new(is_utf8($value) ? $value : decode(latin1 => $value));
	return $value if $width >= $v->columns;          # small enough after counting

	my $r = Unicode::GCString->new(is_utf8($replace) ? $replace : decode(latin1 => $replace));
	$r->columns < $width or return $replace;

	#XXX This is expensive for long texts, but the value could be filled with many zero-widths
	my $take = $width - $r->columns;
	$v->substr(-1, 1, '') while $v->columns > $take;

	# might be one column short
	my $pad = $v->columns + $r->columns < $width ? ' ' : '';
	$v->as_string . $pad . $replace;
}

sub _modif_unknown($$$)
{	my ($self, $format, $value, $args) = @_;
	defined $value or return undef;

	my $defaults = $self->defaults('UNKNOWN');
	$format =~ m/^ UNKNOWN\( ([0-9]+) \) | UNKNOWN\b /x or die $format;
	$args->{width} = $1 // $args->{width} // $defaults->{width};

	return ref $value
		if blessed $value;

	my $trim    = $args->{trim} // $defaults->{trim};
	my $trimmer = $trim eq 'EL' ? '_modif_ellipsis' : $trim eq 'CHOP' ? '_modif_chop' : die $trim;
	my $shorten = sub { $self->$trimmer($trim, $_[0], $args) };

	my $serial  = Data::Dumper->new([$value])->Quotekeys(0)->Terse(1)->Useqq(1)->Indent(0)->Sortkeys(1)->Dump;

	  ! reftype $value          ?  '"' . $shorten->($serial =~ s/^\"//r =~ s/\"$//r) . '"'
	: reftype $value eq 'ARRAY' ?  '[' . $shorten->($serial =~ s/^\[//r =~ s/\]$//r) . ']'
	: reftype $value eq 'HASH'  ?  '{' . $shorten->($serial =~ s/^\{//r =~ s/\}$//r) . '}'
	:     $shorten->($serial);
}

=function printi [$fh], $format, %data|\%data
Calls M<sprinti()> to fill the %data into $format, and then sends it to
the $fh (by default the selected file handle)

  open my $fh, '>:encoding(UTF-8)', $file;
  printi $fh, ...

  printi \*STDERR, ...

=cut

sub printi($$@)
{	my $self = shift;
	my $fh   = ref $_[0] eq 'GLOB' ? shift : select;
	$fh->print($self->sprinti(@_));
}


=function printp [$fh], $format, @positionals, %options
Calls M<sprintp()> to fill the @positionals in $format, and
then sends it to the $fh (by default the selected file handle).
=cut

sub printp($$@)
{	my $self = shift;
	my $fh   = ref $_[0] eq 'GLOB' ? shift : select;
	$fh->print($self->sprintp(@_));
}

=function sprintp $format, @positionals, %options
Where M<sprinti()> uses named parameters --especially useful when the
strings need translation-- this function stays close to the standard
C<sprintf()>.  All features of POSIX formats are supported.  This
should say enough: you can use C<< %3$0#5.*d >>, if you like.

It may be useful to know that the positional $format is rewritten and
then fed into M<sprinti()>.  B<Be careful> with the length of the @positionals:
superfluous parameter %options are passed along to C<sprinti()>, and
should only contain "specials": parameter names which start with '_'.

=example of the rewrite

  # positional parameters
  my $x = sprintp "dumpfiles: %s\n", \@dumpfiles, _join => ':';

  # is rewritten into, and then processed as
  my $x = sprinti "dumpfiles: {_1}\n", _1 => \@dumpfiles, _join => ':';

=cut

sub _printp_rewrite($)
{	my @params = @{$_[0]};
	my $printp = $params[0];
	my ($printi, @iparam);
	my ($pos, $maxpos) = (1, 1);

	while(length $printp)
	{	$printp  =~ s/^([^%]*)//s;  # take printables
		$printi .= $1;
		length $printp or last;

		if($printp =~ s/^\%\%//)    # %% means real %
		{	$printi .= '%';
			next;
		}

		$printp =~ s/
			\%
			(?:([0-9]+)\$)?     # 1=positional
			([-+0 \#]*)         # 2=flags
			([0-9]*|\*)?        # 3=width
			(?:\.([0-9]*|\*))?  # 4=precission
			(?:\{ ([^}]*) \})?  # 5=modifiers
			(\w)                # 6=conversion
		//x
			or die "format error at '$printp' in '$params[0]'";

		$pos       = $1 if $1;
		my $width  = !defined $3 ? '' : $3 eq '*' ? $params[$pos++] : $3;
		my $prec   = !defined $4 ? '' : $4 eq '*' ? $params[$pos++] : $4;
		my $modif  = !defined $5 ? '' : $5;
		my $valpos = $pos++;
		$maxpos    = $pos if $pos > $maxpos;
		push @iparam, "_$valpos" => $params[$valpos];
		my $format = '%'.$2.($width || '').($prec ? ".$prec" : '').$6;
		$format    = '' if $format eq '%s';
		my $sep    = $modif.$format =~ m/^\w/ ? ' ' : '';
		$printi   .= "{_$valpos$sep$modif$format}";
	}
	splice @params, 0, $maxpos, @iparam;
	($printi, \@params);
}

sub sprintp(@)
{	my $self = shift;
	my ($i, $iparam) = _printp_rewrite \@_;
	$self->sprinti($i, +{@$iparam});
}

#--------------------
=chapter DETAILS

  Your manual-page reader may not support the unicode used
  in some of the examples below.

=section Why use C<printi()> to replace C<printf()>?

The C<printf()> function is provided by Perl's CORE; you do not need
to install any module to use it.  Why would you use consider using
this module?

=over 4

=item translating
C<printf()> uses positional parameters, where M<printi()> uses names
to refer to the values to be filled-in.  Especially in a set-up with
translations, where the format strings get extracted into PO-files,
it is much clearer to use names.  This is also a disadvantage of
M<printp()>

=item pluggable serializers
C<printi()> supports serialization for specific data-types: how to
interpolate undef, HASHes, etc.

=item pluggable modifiers
Especially useful in context of translations, the FORMAT string may
contain (language specific) helpers to insert the values correctly.

=item correct use of utf8
Sized string formatting in C<printf()> is broken: it takes your string
as bytes, not Perl strings (which may be utf8).  In unicode, one
"character" may use many bytes.  Also, some characters are displayed
double wide, for instance in Chinese.  The M<printi()> implementation
will use Unicode::GCString for correct behavior.

=item automatic output encoding (for HTML)
You can globally declare that all produced strings must be encoded in
a certain format, for instance that HTML entities should be encoded.

=back

=section Four components

To fill-in a FORMAT, four clearly separated components play a role:

=over 4
=item 1. modifiers
How to change the provided values, for instance to hide locale
differences.
=item 2. serializer
How to represent (the modified) the values correctly, for instance undef
and ARRAYs.
=item 3. conversion
The standard UNIX format rules, like C<%d>.  One conversion rule
has been added 'S', which provides unicode correct behavior.
=item 4. encoding
Prepare the output for a certain syntax, like HTML.
=back

Simplified:

  # sprinti() replaces "{$key$modifiers$conversion}" by
  $encode->($format->($serializer->($modifiers->($args{$key}))))

  # sprintp() replaces "%pos{$modifiers}$conversion" by
  $encode->($format->($serializer->($modifiers->($arg[$pos]))))

Example:

  printi "price: {price € %-10s}", price => $cost;
  printi "price: {price € %-10s}", { price => $cost };
  printp "price: %-10{€}s", $cost;

  $value      = $cost (in €)
  $modifier   = convert € to local currency £
  $serializer = show float as string
  $format     = column width %-10s
  $encode     = £ into &pound;     # when encodingFor('HTML')


=section Interpolation: keys

A key is a bareword (like a variable name) or a list of barewords
separated by dots (no blanks!)

B<Please> use explanatory key names, to help the translation
process once you need that (in the future).

=subsection Simple keys

A simple key directly refers to a named parameter of the function or method:

  printi "Username: {name}", name => 'John';

You may also pass them as HASH or CODE:

  printi "Username: {name}", { name => 'John' };
  printi "Username: {name}", name => sub { 'John' };
  printi "Username: {name}", { name => sub { 'John' } };
  printi "Username: {name}", name => sub { sub {'John'} };

The smartness of pre-processing CODE is part of serialization.

=subsection Complex keys

[0.91] In the previous section, we kept our addressing it simple: let's
change that now.  Two alternatives for the same:

  my $user = { name => 'John' };
  printi "Username: {name}", name => $user->{name}; # simple key
  printi "Username: {user.name}", user => $user;    # complex key

The way these complex keys work, is close to the flexibility of
template toolkit: the only thing you cannot do, is passing parameters
to called CODE.

You can pass a parameter name as HASH, which contains values.  This may
even be nested into multiple levels.  You may also pass objects, class
(package names), and code references.

In above case of C<user.name>, when C<user> is a HASH it will take the
value which belongs to the key C<name>.  When C<user> is a CODE, it will
run code to get a value.  When C<user> is an object, the method C<name>
is called to get a value back.  When C<user> is a class name, the C<name>
refers to an instance method on that class.

More examples which do work:

  # when name is a column in the database query result
  printi "Username: {user.name}", user => $sth->fetchrow_hashref;

  # call a sub which does the database query, returning a HASH
  printi "Username: {user.name}", user => sub { $db->getUser('John') };

  # using an instance method (object)
  { package User;
    sub new  { bless { myname => $_[1] }, $_[0] }
    sub name { $_[0]->{myname} }
  }
  my $user = User->new('John');
  printi "Username: {user.name}", user => $user;

  # using a class method
  sub User::count   { 42 }
  printi "Username: {user.count}", user => 'User';

  # nesting, mixing
  printi "Complain to {product.factory.address}", product => $p;

  # mixed, here CODE, HASH, and Object
  printi "Username: {document.author.name}", document => sub {
    return +{ author => User->new('John') }
  };

Limitation: you cannot pass arguments to CODE calls.


=section Interpolation: Serialization

The 'interpolation' functions have named VARIABLES to be filled-in, but
also additional OPTIONS.  To distinguish between the OPTIONS and VARIABLES
(both a list of key-value pairs), the keys of the OPTIONS start with
an underscore C<_>.  As result of this, please avoid the use of keys
which start with an underscore in variable names.  On the other hand,
you are allowed to interpolate OPTION values in your strings.

There is no way of checking beforehand whether you have provided all
values to be interpolated in the translated string.  When you refer to
value which is missing, it will be interpreted as undef.

=over 4

=item strings
Simple scalar values are interpolated "as is"

=item CODE
When a value is passed as CODE reference, that function will get called
to return the value to be filled in.
For interpolating, the following rules apply:

=item SCALAR
Takes the value where the scalar reference points to.

=item ARRAY
All members will be interpolated with C<,␣> between the elements.
Alternatively (maybe nicer), you can pass an interpolation parameter
via the C<_join> OPTION.

  printi "matching files: {files}", files => \@files, _join => ', '

=item HASH
By default, HASHes are interpolated with sorted keys,

  $key => $value, $key2 => $value2, ...

There is no quoting on the keys or values (yet).  Usually, this will
produce an ugly result anyway.

=item Objects
With the C<serialization> parameter, you can overrule the interpolation
of above defaults, but also add rules for your own objects.  By default,
objects get stringified.

  serialization => [ $myclass => \&name_in_reverse ]

  sub name_in_reverse($$$)
  {   my ($formatter, $object, $args) = @_;
      # the $args are all parameters to be filled-in
      scalar reverse $object->name;
  }

=back

=section Interpolation: Modifiers

Modifiers are used to change the value to be inserted, before
the characters get interpolated in the line.  This is a powerful
simplification.  Some useful modifiers are already provided by default.
They are also good examples how to write your own.

Let's discuss this with an example.
In traditional (gnu) gettext, you would write:

  printf(gettext("approx pi: %.6f\n"), PI);

to get PI printed with six digits in the fragment.
Locale::TextDomain has two ways to achieve that:

  printf __"approx pi: %.6f\n", PI;
  print __x"approx pi: {approx}\n", approx => sprintf("%.6f", PI);

The first does not respect the wish to be able to reorder the arguments
during translation (although there are ways to work around that)  The
second version is quite long.  The string to be translated differs
between the two examples.

With C<Log::Report>, above syntaxes do work as well, but you can also do:

  # with optional translations
  print __x"approx pi: {pi%.6f}\n", pi => PI;

The base for C<__x()> is the M<printi()> provided by this module. Internally,
it will call C<printi> to fill-in parameters:

  printi "approx pi: {pi%.6f}\n", pi => PI;

Another example:

  printi "{perms} {links%2d} {user%-8s} {size%10d} {fn}\n",
     perms => '-rw-r--r--', links => 7, user => 'me',
     size => 12345, fn => $filename;

An additional advantage (when you use translation) is the fact that not
all languages produce comparable length strings.  Now, the translators
can change the format, such that the layout of tables is optimal for their
language.

Above example in M<printp()> syntax, shorter but less maintainable:

  printp "%s %2d %-8s 10d %s\n",
     '-rw-r--r--', 7, 'me', 12345, $filename;

=subsection Modifier: POSIX format starts with '%'

As shown in the examples above, you can specify a format.  This can,
for instance, help you with rounding or columns:

  printp "π = {pi%.3f}", pi => 3.1415;
  printp "weight is {kilogram%d}", kilogram => 127*OUNCE_PER_KILO;
  printp "{filename%-20.20s}\n", filename => $fn;

=subsubsection POSIX modifier extension '%S'

The POSIX C<printf()> does not handle unicode strings.  Perl does
understand that the 's' modifier may need to insert utf8 so does not
count bytes but characters.  M<printi()> does not use characters but
"grapheme clusters" via Unicode::GCString.  Now, also composed
characters do work correctly.

Additionally, you can use the B<new 'S' conversion> to count in columns.
In fixed-width fonts, graphemes can have width 0, 1 or 2.  For instance,
Chinese characters have width 2.  When printing in fixed-width, this
'S' is probably the better choice over 's'.  When the field does not
specify its width, then there is no performance penalty for using 'S'.

  # name right aligned, commas on same position, always
  printp "name: {name%20S},\n", name => $some_chinese;

=subsubsection POSIX modifier extensions '%d'

The full pattern is pretty complex: C<< %[+ -0]?[0-9]*[_,.]?d >>, POSIX
format style with a few extra features.  The middle (optional) digits tell
the minimal size of the integer to be displayed.  The optional character
before it says: C<+> will add a '+' sign when positive, 'blank' adds one
blank before the digits when positive, a C<0> pads left with zeros instead
of blanks.  Finally, a C<-> means left-align padding blanks on the right.

[0.96] The last pattern is only useful when you print (big) decimals:
add an underscore, comma, or dot on the thousands.

  printi "{count%_d}\n", count => 1e9;   # 1_000_000_000
  printi "{count%,d}\n", count => 1e9;   # 1,000,000,000
  printi "{count%.d}\n", count => 1e9;   # 1.000.000.000

  printi "'{v%10.d}'",  v =>  10000;   # '    10.000';
  printi "'{v%10_d}'",  v => -10000;   # '   -10_000';
  printi "'{v%-10.d}'", v =>  10000;   # '10.000    ';
  printi "'{v%-10.d}'", v => -10000;   # '-10.000   ';
  printi "'{v%+10,d}'", v =>  10000;   # '   +10,000';
  printi "'{v% ,d}'",   v =>  10000;   # ' 10,000';
  printi "'{v% ,d}'",   v => -10000;   # '-10,000';

[1.00] You can set the default C<FORMAT> separator:

  use String::Print
      defaults => [ FORMAT => { thousands => ',' } ];

  # or
  my $sp = String::Print->new(
     defaults => { FORMAT => { thousands => ',' }},
  );

  # or
  $sp->setDefaults(FORMAT => { thousands => ',' });

=subsection Modifier: BYTES

[0.91] Too often, you have to translate a (file) size into humanly readible
format.  The C<BYTES> modifier simplifies this a lot:

  printp "{size BYTES} {fn}\n", fn => $fn, size => -s $fn;

The output will always be 5 characters.  Examples are "999 B", "1.2kB",
and " 27MB".

=subsection Modifier: HTML

[0.95] interpolate the parameter with HTML entity encoding.

=subsection Modifiers: YEAR, DATE, TIME, and DT()

[0.91] A set of modifiers help displaying dates and times.  They are a
little flexible in values they accept, but do not expect miracles: when
it get harder, you will need to process it yourself.

The actual treatment of a time value depends on the value.  Four
different situations:

=over 4

=item 1. numeric
A pure numeric value is considered "seconds since epoch", unless it
is smaller than 21000000, in which case it is taken as date without
separators.

=item 2. M<DateTime> object
[1.00] Use a M<DateTime> object to provide the value.  This way, the
format does not need to know whether the date is specified as object or
as string.

  my $now = DateTime->now;
  printi "{t YEAR}", t => $now;  # works for DateTime, epoch and string
  printi "{t.year YEAR}", t => $now;  # same effect, DateTime only
  printi "{t.year}", t => $now;  # will also work, not as nice

=item 3. date format without time-zone
The same formats are understood as in the next option, but without
time-zone information.  The date is processed as text as if in the
local time zone, and the output in the local time-zone.

=item 4. date format with time-zone

By far not all possible date formats are supported, just a few common
versions, like

  2017-06-27 10:04:15 +02:00
  2017-06-27 17:34:28.571491+02  # psql timestamp with zone
  20170627100415+2
  2017-06-27T10:04:15Z           # iso 8601
  20170627                       # only for YEAR and DATE
  2017-6-1                       # only for YEAR and DATE
  12:34                          # only for TIME

The meaning of C<05-04-2017> is ambiguous, so not supported.  Milliseconds
get ignored.

When the provided value has a timezone indication, it will get
converted into the local timezone of the observer.

=back

The output of C<YEAR> is in format 'YYYY', for C<DATE> it will always be
'YYYY-MM-DD', where C<TIME> produces 'HH:mm:ss'.

The DT modifier can produce different formats:

  DT(ASC)     : %a %b %e %T %Y       asctime output
  DT(FT)      : %F %T                YYYY-MM-DD HH:MM:SS
  DT(ISO)     : %FT%T%z              iso8601
  DT(RFC822)  : %a, %d %b %y %T %z   email old
  DT(RFC2822) : %a, %d %b %Y %T %z   email newer
  DT(RFC5322) : %a, %d %b %Y %T %z   email newest [0.96]

You may suggest additional formats, or add your own modifier.

The defaults are:

  DT => { standard => 'FT' },

=subsection Modifier: //word, //"string", //'string'

[0.91] By default, an undefined value is shown as text 'undef'.  Empty
strings are shown as nothing.  This may not be nice.  You may want to
be more specific when a value is missing.

  "visitors: {count //0}"
  "published: {date DT//'not yet'}"
  "copyright: {year//2017 YEAR}

Modifiers will usually return undef when they are called with an
undefined or empty value.  By the right order of '//', you may product
different kinds of output:

  "price: {price//5 EUR}"
  "price: {price EUR//unknown}"

=subsection Modifier: '=' (show name)

[0.96] As (always trailing) modifier, this will show the interpolated
name before the value.  It might simplify debugging statements.

  "visitors: {count=}", count => 1;      # visitors: count=1
  "v: {count %-8,d =}X", count => 10000; # v: count=10,000␣␣X

=subsection Modifier: EL or EL($width, [$replace])

[1.00] When the string is larger than C<$width> columns, then chop it
short and add a 'mid-line ellipsis' character: C< ⋯  >.  You may also
pick another replacement string.   The comma is optional

Attention: "columns" not "characters": it is aware of wide fonts, like
chinese characters (see C<%S> above).  The default ellipsis is also two
columns wide.

  "Intro: {text EL(10)}";     # Intro: 12345678⋯ 
  "Intro: {text EL(10,⋮)}";   # Intro: 123456789⋮ 
  "Intro: {text EL(10⋮)}";    # Intro: 123456789⋮ 
  "Intro: {text EL(10,XY)}";  # Intro: 12345678XY 
  "Intro: {text EL(10XY)}";   # Intro: 12345678XY 

The defaults for EL are '20' and '⋯' (mid-dots).  You can changes these
with M<setDefaults()>:

  $sp->setDefaults(EL => { width => 10, replace => '⋮' });
  $sp->printi("Intro: {text EL}"); # Intro: 12345678⋯ 

=subsection Modifier: CHOP or CHOP($width, [$units])

[1.00] When the string is larger than C<$width> columns (defaults to
20), then chop it short and add C<< [+42] >>: the number of character
chopped off.  The C<$width> is the size of the result string.
The comma is optional.

  "Intro: {text CHOP(10)}";        # Intro: 12345[+42]
  "Intro: {text CHOP(19 chars)}";  # Intro: 1234578[+33 chars]
  "Intro: {text CHOP(19, chars)}"; # Intro: 1234578[+33 chars]

The same effect can be reached by setting the defaults

  $sp->setDefaults(CHOP => +{width => 19, units => ' chars'});
  $sp->printi("Intro: {text CHOP}", text => $t); # Intro: 1234578[+33 chars]

Other defaults are C<head> (C<<[>>) and C<tail> (C<<]>>).

  $sp->setDefaults(CHOP => +{head => '«', tail => '»'});
  $sp->printi("Intro: {text CHOP}", text => $t); # Intro: 1234578«+12»

=subsection Modifier: UNKNOWN or UNKNOWN($width)

[1.01] When you need to interpolate a value which is unknown and potentially
unsafe, then use this.  For instance, you produce an internal error message
to report that a method is used incorrectly:

  sub openFolder
  {  my ($self, $folder) = @_;

     blessed $folder && $folder->isa('Mail::Box')
        or printi "ERROR: expected a Mail::Box, got a {t UNKNOWN}.",
              t => $folder;
  }

Now, this C<$folder> parameter is clearly wrong.  But what is it?  Did we
pass a wrong object type?  Did we pass a string?  An ARRAY maybe?

The C<UNKNOWN> modifier distibuishes the following:
=over 4
=item * undefined values will be shown as the UNDEF serializer does;
=item * objects will be represented by their type;
=item * references will show the first part of their Data::Dumper dump; and
=item * the leading parts of strings will be shown between double quotes.
=back

Care is taken that weird characters are escaped.  The shortening uses the
C<trim> setting: either C<'EL'> (default) or C<'CHOP'>.

=subsection Private modifiers

You may pass your own modifiers.  A modifier consists of a selector and
a CODE, which is called when the selector matches.  The selector is either
a string or a regular expression.

  # in Object Oriented syntax:
  my $f = String::Print->new(
    modifiers => [ qr/[€₤]/ => \&money ],
  );

  # in function syntax:
  use String::Print 'printi', 'sprinti',
    modifiers => [ qr/[€₤]/ => \&money ];

  # the implementation:
  sub money$$$$)
  { my ($formatter, $modif, $value, $args) = @_;

      $modif eq '€' ? sprintf("%.2f EUR", $value+0.0001)
    : $modif eq '₤' ? sprintf("%.2f GBP", $value/1.16+0.0001)
    :                 'ERROR';
  }

Using M<printp()> makes it a little shorter, but will become quite
complex when there are more parameter in one string.

  printi "price: {p€}", p => $pi;   # price: 3.14 EUR
  printi "price: {p₤}", p => $pi;   # price: 2.71 GBP

  printp "price: %{€}s", $pi;       # price: 3.14 EUR
  printp "price: %{₤}s", $pi;       # price: 2.71 GBP

This is very useful in the translation context, where the translator can
specify abstract formatting rules.  As example, see the (GNU) gettext
files, in the translation table for Dutch into English.  The translator
tells us which currency to use in the display.

  msgid  "kostprijs: {p€}"
  msgstr "price: {p₤}"

Another example.  Now, we want to add timestamps.  In this case, we
decide for modifier names in C<\w>, so we need a blank to separate
the parameter from the modifer.

=subsection Stacking modifiers

You can add more than one modifier.  The modifiers detect the extend of
their own information (via a regular expression), and therefore the
formatter understands where one ends and the next begins.

The modifiers are called in order:

  printi "price: {p€%9s}\n", p => $p; # price: ␣␣␣123.45
  printi "!{t TIME%10s}!", t => $now; # !␣␣12:59:17!

  printp "price: %9{€}s\n", $p;       # price: ␣␣␣123.45
  printp "!%10{TIME}s!", $now;        # !␣␣12:59:17!

=section Output encoding

[0.91] This module is also used by Log::Report::Template, which is used
to insert (translated) strings with parameters into HTML templates.
You can imagine that some of the parameter may need to be encoded to
HTML in the template, and other not.

=subsection Example with Log::Report::Template

In pure Template Toolkit, you would write

  # in your TT-template
  <div>Username: [% username | html %]</div>
  # in your code
  username => $user->name,

With plain String::Print with output encoding enabled, you can do:

  # in your TT-template
  <div>[% show_username %]</div>
  # in your code with encodeFor('HTML')
  show_username => printi("Username: {user}", user => $user->name),
  # or
  show_username => printp("Username: %s", $user->name),

That does not look very efficient, however it changes for the good when
this is combined with L<Log::Report::Lexicon> (translations)  You can
either do:

  # in your TT-template
  <div>[% show_username %]</div>
  # in your code with encodeFor('HTML')
  show_username => __x("Username: {user}", user => $user->name),

Shorter:

  # in your TT-template with encodeFor('HTML')
  <div>[% loc("Username: {user}", user => username) %]</div>
  # in your code
  username => $user->name,

Even shorter:

  # in your TT-template with encodeFor('HTML')
  <div>[% loc("Username: {user.name}", user => userobj) %]</div>
  # in your code
  userobj => $user,

Shortest:

  # in your TT-template with encodeFor('HTML')
  <div>[% loc("Username: {user.name}") %]</div>
  # in your code
  user => $user,

Shorter that the original, and translations for free!
More examples in Log::Report::Template.

=subsection Output encoding exclusion

In some cases, the data which is inserted is already encoded in the
output syntax.  For instance, you already have HTML to be included.

The default exclusion rule for HTML output is C<< qr/html$/i >>, which
means that all inserted named parameters, where the name ends on C<html>
will not get html-entity encoded.

This will work by default:

  # with encodeFor('HTML')
  printp "Me & Co: {name}, {description_html}",
     name => 'RenE<eacute>', description_html => $descr;

This may result in:

  Me E<amp>amp; Co: RenE<amp>eacute;, <font color="red">new member</font>

Better not to have HTML in your program: leave it to the template.  But
in some cases, you have no choice.


=section String::Print compared to other modules on CPAN

There are a quite a number of modules on CPAN which extend the functionality
of C<printf()>.  To name a few:
L<String::Format|https://metacpan.org/dist/String-Format>,
L<String::Errf|https://metacpan.org/dist/String-Errf>,
L<String::Formatter|https://metacpan.org/dist/String-Formatter>,
L<Text::Sprintf::Named|https://metacpan.org/dist/Text-Sprintf-Named>,
L<Acme::StringFormat|https://metacpan.org/dist/Acme-StringFormat>,
L<Text::sprintf|https://metacpan.org/dist/Text-sprintfn>,
L<Log::Sprintf|https://metacpan.org/dist/Log-Sprintf>, and
L<String::Sprintf|https://metacpan.org/dist/String-Sprintf>.
They are all slightly different.

When the C<String::Print> module was created, none of the modules
mentioned above handled unicode correctly.  Global configuration
of serializers and modifiers is also usually not possible, sometimes
provided per explicit function call.  Only C<String::Print> cleanly
separates the roles of serializers, modifiers, and conversions.

C<String::Print> is nicely integrated with Log::Report.

=cut

1;
