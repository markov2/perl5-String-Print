use warnings;
use strict;

package String::Print;

#use Log::Report::Optional 'log-report';

use Encode            qw/is_utf8/;
use Unicode::GCString ();

my @default_modifiers   = ( qr/%\S+/ => \&_format_printf );
my %default_serializers =
 ( UNDEF     => sub { 'undef' }
 , ''        => sub { $_[1]   }
 , ARRAY     =>
     sub { my $v = $_[1]; my $join = $_[2]{_join} // ', ';
           join $join, map +($_ // 'undef'), @$v;
         }
 , HASH      =>
     sub { my $v = $_[1];
           join ', ', map "$_ => ".($v->{$_} // 'undef'), sort keys %$v;
         }
 # CODE value has different purpose
 );

=encoding utf8

=chapter NAME
String::Print - printf alternatives

=chapter SYNOPSIS

 ### Functional interface
 use String::Print qw/printi printp/, %config;

 # interpolation of arrays and hashes
 printi 'age {years}', years => 12;
 printi 'price-list: {prices%.2f}', prices => \@prices;
 printi 'dump: {hash}', hash => \%config;

 # same with positional parameters
 printp 'age %d", 12;
 printp 'price-list: %.2f', \@prices;
 printp 'dump: %s', \%settings;

 ### Object Oriented interface
 use String::Print 'oo';      # import nothing 
 my $f = String::Print->new(%config);

 # same, called directly
 $f->printi('age {years}', years => 12);
 $f->printp('age %d', 12);

 ### via Log::Report's __* functions
 use Log::Report::Optional;
 print __x"age {years}", years => 12;

=chapter DESCRIPTION
This module inserts values into (translated) strings.  It provides
C<printf> and C<sprintf> alternatives via both an object oriented and a
functional interface.

Read in the L<DETAILS> chapter below, why this module provides a better
alternative for C<printf()>.  Also, some examples can be found there.
Take a look at them first!

=chapter METHODS

=section The Object Oriented interface

See functions M<printi()>, M<sprinti()>, M<printp()>, and M<sprintp()>: you
can also call them as method.  Same functionality:

  use String::Print 'oo';
  my $f = String::Print->new(%config);
  $f->printi($format, @params);

  use String::Print 'printi', %config;
  printi $format, @params;

The Object Oriented interface wins when you need the same configuration
in multiple source files, or when you need different configurations
within one program.

=section Constructors

=c_method new OPTIONS

=option  modifiers ARRAY
=default modifiers C<[ qr/^%\S+/ => \&format_printf]>
Add one or more modifier handlers to power of the formatter.  They will
get preference over the predefined modifiers, but lower than the modifiers
passed to C<print[ip]> itself.

=option  serializers HASH|ARRAY
=default serializers <useful defaults>
How to serialize data elements.

=examples

  my $f = String::Print->new
    ( modifiers   => [ EUR   => sub {sprintf "%5.2f e", $_[0]} ]
    , serializers => [ UNDEF => sub {'-'} ]
    );

  $f->printi("price: {p EUR}", p => 3.1415); # price: ␣␣3.14 e
  $f->printi("count: {c}", c => undef);      # count: -
=cut

sub new(@) { my $class = shift; (bless {}, $class)->init( {@_} ) }
sub init($)
{   my ($self, $args) = @_;

    my $modif = $self->{LRF_modif} = [ @default_modifiers ];
    if(my $m  = $args->{modifiers})
    {   unshift @$modif, @$m;
    }

    my $s    = $args->{serializers} || {};
    my $seri = $self->{LRF_seri}
      = { %default_serializers, (ref $s eq 'ARRAY' ? @$s : %$s) };

    $self;
}

sub import(@)
{   my $class = shift;
    my ($oo, %func);
    while(@_)
    {   last if $_[0] !~ m/^s?print[ip]$/;
        $func{shift()} = 1;
    }

    if(@_ && $_[0] eq 'oo')   # only object oriented interface
    {   shift @_;
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
}

#-------------
=section Attributes
=method addModifiers PAIRS
=cut

sub addModifiers(@) {my $self = shift; unshift @{$self->{LRF_modif}}, @_}

#-------------------
=chapter FUNCTIONS

The functional interface creates a hidden object.  You may import any of
these functions explicitly, or all together by not specifying the names.

Examples:

  use String::Print;           # all
  use String::Print 'sprinti'; # only sprinti

  use String::Print 'printi'   # only printi
    , modifiers   => [ EUR   => sub {sprintf "%5.2f e", $_[0]} ]
    , serializers => [ UNDEF => sub {'-'} ];

  printi "price: {p EUR}", p => 3.1415; # price: ␣␣3.14 e
  printi "count: {c}", c => undef;      # count: -

=function sprinti FORMAT, PAIRS|HASH
The FORMAT refers to some string, maybe the result of a translation.

The PAIRS (which may be passed as LIST or HASH) contains a mixture of
special and normal variables to be filled in.  The names of the special
variables (the options) start with an underscore (C<_>).

=option  _count INTEGER
=default _count C<undef>
Result of the translation process: when M<Log::Report::__xn()> is
are used for count-sensitive translation.  Those function may add
more specials to the parameter list.

=option  _join STRING
=default _join ', '
Which STRING to be used when an ARRAY is being filled-in.

=option  _prepend STRING
=default _prepend C<undef>
Text as STRING prepended before FORMAT, without interpolation.

=option  _append  STRING
=default _append  C<undef>
Text as STRING appended after FORMAT, without interpolation.

=cut

sub sprinti($@)
{   my ($self, $format) = (shift, shift);
    my $args = @_==1 ? shift : {@_};

    $args->{_join} //= ', ';

    my $result = $format;
    $result    =~ s/\{(\w+)\s*([^}]*?)\s*\}/$self->_expand($1,$2,$args)/ge;

    $result    = $args->{_prepend} . $result if defined $args->{_prepend};
    $result   .= $args->{_append}            if defined $args->{_append};
    $result;
}

sub _expand($$$)
{   my ($self, $key, $modifier, $args) = @_;
    my $value = $args->{$key};

    $value = $value->($self, $key, $args)
        while ref $value eq 'CODE';

    my $mod;
 STACKED:
    while(length $modifier)
    {   my @modif = @{$self->{LRF_modif}};
        while(@modif)
        {   my ($regex, $callback) = (shift @modif, shift @modif);
            $modifier =~ s/^($regex)\s*// or next;

            $value = $callback->($self, $1, $value, $args);
            next STACKED;
        }
        return "{unknown modifier '$modifier'}";
    }

    my $seri = $self->{LRF_seri}{defined $value ? ref $value : 'UNDEF'};
    $seri ? $seri->($self, $value, $args) : "$value";
}

# See dedicated section in explanation in DETAILS
sub _format_printf($$$$)
{   my ($self, $format, $value, $args) = @_;

    # be careful, often $format doesn't eat strings
    defined $value
        or return 'undef';

    use locale;
    if(ref $value eq 'ARRAY')
    {   @$value or return '(none)';
        return [ map $self->_format_print($format, $_, $args), @$value ] ;
    }
    elsif(ref $value eq 'HASH')
    {   keys %$value or return '(none)';
        return { map +($_ => $self->_format_print($format,$value->{$_},$args)),
                   keys %$value } ;

    }

    return sprintf $format, $value
        unless is_utf8 $value
            && $format =~ m/^\%([-+ ]?)([0-9]*)(?:\.([0-9]*))?([sc])$/;

    # String formats like %10s or %-3.5s count characters, no width.
    # String formats like %10c or %-3.5c are subject to column width.
    # The latter means: minimal 3 chars, max 5, padding right with blanks.

    my ($padding, $width, $max, $u) = ($1, $2, $3, $4);
    my $s = Unicode::GCString->new($value);
    my $pad;

    if($u eq 'c')
    {   # too large to fit
        return $value if !$max && $width && $width <= $s->columns;

        # wider than max.  Waiting for $s->trim($max) if $max
        $s->substr(-1, 1, '')
           while $max && $s->columns > $max;

        $pad = $width ? $width - $s->columns : 0;
    }
    else  # $u eq 's'
    {   return $value if !$max && $width && $width <= length $s;
        $s->substr($max, length($s)-$max, '') if $max && length $s > $max;
        $pad = $width ? $width - length $s : 0;
    }

      $pad==0         ? $s->as_string
    : $padding eq '-' ? $s->as_string . (' ' x $pad)
    :                   (' ' x $pad) . $s->as_string;
}

=function printi [FILEHANDLE], FORMAT, PAIRS|HASH
Calls M<sprinti()> to fill the data in PAIRS or HASH in FORMAT, and
then sends it to the FILEHANDLE (by default the selected file)

  open my $fh, '>', $file;
  printi $fh, ...

  printi \*STDERR, ...

=cut

sub printi($$@)
{   my $self = shift;
    my $fh   = ref $_[0] eq 'GLOB' ? shift : select;
    $fh->print($self->sprinti(@_));
}

=function printp [FILEHANDLE], FORMAT, PAIRS|HASH
Calls M<sprintp()> to fill the data in PAIRS or HASH in FORMAT, and
then sends it to the FILEHANDLE (by default the selected file)
=cut

sub printp($$@)
{   my $self = shift;
    my $fh   = ref $_[0] eq 'GLOB' ? shift : select;
    $fh->print($self->sprintp(@_));
}

=function sprintp FORMAT, LIST, PAIRS
Where M<sprinti()> uses named parameters --especially useful when the
strings need translation-- this function stays close to the standard
C<sprintf()>.  All features of POSIX formats are supported.  This
should say enough: you can use C<%3$0#5.*d>, if you like.

It may be useful to know that the positional FORMAT is rewritten and
then fed into M<sprinti()>.  Be careful with the length of the LIST:
superfluous parameter PAIRS are passed along to C<sprinti()>, and
should only contain "specials".

=example

  # positional parameters
  my $x = sprintp "dumpfiles: %s\n", \@dumpfiles
     , _join => ':';

  # rewriten and processed as
  my $x = sprinti "dumpfiles: {filenames}\n"
     , filenames => \@dumpfiles, _join => ':';

=cut

sub _printp_rewrite($)
{   my @params = @{$_[0]};
    my $printp = $params[0];
    my ($printi, @iparam);
    my ($pos, $maxpos) = (1, 1);
    while(length $printp && $printp =~ s/^([^%]+)//s)
    {   $printi .= $1;
        length $printp or last;
        if($printp =~ s/^\%\%//)
        {   $printi .= '%';
            next;
        }
        $printp =~ s/\%(?:([0-9]+)\$)?     # 1=positional
                       ([-+0 \#]*)         # 2=flags
                       ([0-9]*|\*)?        # 3=width
                       (?:\.([0-9]*|\*))?  # 4=precission
                       (?:\{ ([^}]*) \})?  # 5=modifiers
                       (\w)                # 6=conversion
                    //x
            or die "format error at '$printp' in '$params[0]'";

        $pos      = $1 if $1;
        my $width = !defined $3 ? '' : $3 eq '*' ? $params[$pos++] : $3;
        my $prec  = !defined $4 ? '' : $4 eq '*' ? $params[$pos++] : $4;
        my $modif = !defined $5 ? '' : $5;
        my $valpos= $pos++;
        $maxpos   = $pos if $pos > $maxpos;
        push @iparam, "_$valpos" => $params[$valpos];
        my $format= '%'.$2.($width || '').($prec ? ".$prec" : '').$6;
        $format   = '' if $format eq '%s';
        my $sep   = $modif.$format =~ m/^\w/ ? ' ' : '';
        $printi  .= "{_$valpos$sep$modif$format}";
    }
    splice @params, 0, $maxpos, @iparam;
    ($printi, \@params);
}

sub sprintp(@)
{   my $self = shift;
    my ($i, $iparam) = _printp_rewrite \@_;
    $self->sprinti($i, {@$iparam});
}

#-------------------
=chapter DETAILS

=section Why C<printi()>, not C<printf()>?

The C<printf()> function is provided by Perl's CORE; you do not need
to install any module to use it.  Why would you use consider using
this module?

=over 4

=item translating
C<printf()> uses positional values, where M<printi()> uses names
to refer to the values to be filled-in.  Especially in a set-up with
translations, where the format strings get extracted into PO-files,
it is much clearer to use names.  This is also a disadvantage of
M<printp()>

=item pluggable serializers
C<printi()> supports serialization for specific data-types: how to
interpolate C<undef>, HASHes, etc.

=item pluggable modifiers
Especially useful in context of translations, the FORMAT string may
contain (language specific) helpers to insert the values correctly.

=item correct use of utf8
Sized string formatting in C<printf()> is broken: it takes your value
as bytes, not Perl strings (maybe unicode).  In Unicode, one character
may use many bytes.  Also, some characters are double wide, for instance
in Chinese.  The M<printi()> implementation will use M<Unicode::GCString>
for correct behavior.

=back

=section Three components

To fill-in a FORMAT, three clearly separated components play a role:

=over 4
=item serializer
How to represent the data correctly, for instance C<undef> and
ARRAYs.
=item modifiers
How to change the serialized data into text, to be inserted in the
FORMAT.
=item conversion
The standard UNIX conversion rules, like C<%d>.  One conversion rule
has been changed 'c', for unicode correct behavior.
=back

=section Interpolation, the serialization of variables

The 'interpolation' functions have named VARIABLES to be filled-in, but
also additional OPTIONS.  To distinguish between the OPTIONS and VARIABLES
(both a list of key-value pairs), the keys of the OPTIONS start with
an underscore C<_>.  As result of this, please avoid the use of keys
which start with an underscore in variable names.  On the other hand,
you are allowed to interpolate OPTION values in your strings.


There is no way of checking beforehand whether you have provided all
values to be interpolated in the translated string.  When you refer to
value which is missing, it will be interpreted as C<undef>.

=over 4

=item CODE
When a value is passed as CODE reference, that function will get called
to return the value to be filled in.
For interpolating, the following rules apply:

=item strings
Simple scalar values are interpolated "as is"

=item ARRAY
All members will be interpolated with C<,␣> between the elements.
Alternatively (maybe nicer), you can pass an interpolation parameter
via the C<_join> OPTION.

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

Modifiers are used to change the value to be inserted, before the characters
get interpolated in the line.

=subsection Modifiers: unix format

Next to the name, you can specify a format code.  With C<gettext()>,
you often see this:

 printf gettext("approx pi: %.6f\n"), PI;

M<Locale::TextDomain> has two ways:

 printf __"approx pi: %.6f\n", PI;
 print __x"approx pi: {approx}\n", approx => sprintf("%.6f", PI);

The first does not respect the wish to be able to reorder the arguments
during translation (although there are ways to work around that)  The
second version is quite long.  The content of the translation table
differs between the examples.

With C<Log::Report>, above syntaxes do work, but you can also do:

 # with optional translations
 print __x"approx pi: {pi%.6f}\n", pi => PI;

The standard M<printi()> is without smart behavior

 printi   "approx pi: {pi%.6f}\n", pi => PI;

Another example:

 printi "{perms} {links%2d} {user%-8s} {size%10d} {fn}\n"
    , perms => '-rw-r--r--', links => 7, user => 'me'
    , size => '12345', fn => $filename;

An additional advantage is the fact that not all languages produce
comparable length strings.  Now, the translators can take care that
the layout of tables is optimal.

Above example in M<printp()> syntax, shorter but less maintainable:

 printp "%s %2d %-8s 10d %s\n"
    , '-rw-r--r--', 7, 'me', '12345', $filename;

=subsection Modifiers: unix format improvements

The POSIX C<printf()> does not handle unicode strings.  Perl does
understand that the 's' modifier may need to insert utf8 so does not
count bytes but characters.  C<printi()> does not use characters but
"grapheme clusters" via M<Unicode::GCString>.  Now, also composed
characters do work correctly.

Additionally, you can use the B<new 'c' conversion> to count in columns.
In fixed-width fonts, graphemes can have width 0, 1 or 2.  For instance,
Chinese characters have width 2.  When printing in fixed-width, this
'c' is probably the better choice over 's'.

=subsection Modifiers: private modifiers

You may pass your own modifiers.  In Object Oriented syntax:

  my $f = String::Print->new
    ( modifiers => [ qr/[€₤]/ => \&money ]
    );

In function syntax

  use String::Print 'printi', 'sprinti'
    , modifiers => [ qr/[€₤]/ => \&money ];

  sub money$$$$)
  { my ($formatter, $modif, $value, $args) = @_;

      $modif eq '€' ? sprintf("%.2f EUR", $value+0.0001)
    : $modif eq '₤' ? sprintf("%.2f PND", $value/1.23+0.0001)
    :                 'ERROR';
  }

Now:

  printi "price: {p€}", p => $pi;   # price: 3.14 EUR
  printi "price: {p₤}", p => $pi;   # price: 2.55 PND

This is very useful in the translation context, where the translator
can specify abstract formatting.  Using M<printp()> makes it a little
shorter, but will become quite complex when there are more parameter
in one string:

  printp "price: %{€}s", $pi;       # price: 3.14 EUR
  printp "price: %{₤}s", $pi;       # price: 2.55 PND

Another example.  Now, we want to add timestamps.  In this case, we
decide for modifier names in C<\w>, so we need a blank to separate
the paramter from the modifer.

  use POSIX  qw/strftime/;
  use String::Print modifiers => [ qr/T|DT|D/ => \&_timestamp ];

  sub _timestamp($$$$)
    { my ($formatter, $modif, $value, $args) = @_;
      my $time_format
        = $modif eq 'T'  ? '%T'
        : $modif eq 'D'  ? '%F'
        : $modif eq 'DT' ? '%TT%FZ'
        :                  'ERROR';
      strftime $time_format, localtime($value);
    };

  printi "time: {t T}",  t => $now;  # time: 12:59:17
  printi "date: {t D }", t => $now;  # date: 2013-04-13
  printi "both: {t DT}", t => $now;  # both: 12:59:17T2013-04-13Z

  printp "time: %{T}s",  $now;       # time: 12:59:17
  printp "date: %{D}s",  $now;       # date: 2013-04-13
  printp "both: %{DT}s", $now;       # both: 12:59:17T2013-04-13Z

=subsection Modifiers: stacking

You can add more than one modifier.  The modifiers detect the extend of
their own information (via a regular expression), and therefore the
formatter understands where one ends and the next begins.

The modifiers are called in order:

  printi "price: {p€%9s}\n", p => $p; # price: ␣␣␣123.45
  printi ">{t T%10s}<", t => $now;    # >␣␣12:59:17<

  printp "price: %9{€}s\n", $p;       # price: ␣␣␣123.45
  printp ">%10{T}s<", $now;           # >␣␣12:59:17<


=section Compared to other modules on CPAN

There are a few more modules on CPAN which extend the functionality
of C<printf()>.  To name a few:
F<http://search.cpan.org/~darren/String-Format|String::Format>,
F<http://http://search.cpan.org/~rjbs/String-Errf|String::Errf>,
F<http://http://search.cpan.org/~rjbs/String-Formatter|String::Formatter>,
F<http://search.cpan.org/~shlomif/Text-Sprintf-Named|Text::Sprintf::Named>,
F<http://search.cpan.org/~gfuji/Acme-StringFormat|Acme::StringFormat>,
F<http://search.cpan.org/~sharyanto/Text-sprintfn|Text::sprintf>,
F<http://search.cpan.org/~frew/Log-Sprintf|Log::Sprintf>, and
F<http://search.cpan.org/~bartl/String-Sprintf|String::Sprintf>.
They are all slightly different.

When the C<String::Print> module got created, none of the mentioned above
natively handled unicode correctly.  Global configuration of serializers,
and modifiers is usually not possible, but only provided per function
call.  Only C<String::Print> cleanly separates the roles of serializers,
modifiers, and conversions.

=cut

1;
