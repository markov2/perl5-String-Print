#!/usr/bin/env perl
# Demonstrate the OO examples

use warnings;
use strict;

use Test::More tests => 7;

use String::Print 'oo';
use POSIX  qw/strftime/;

#
### currency conversion example
#

my $f = String::Print->new
  ( modifiers   => [ EUR   => sub { sprintf "%5.2f e", $_[2] } ]
  , serializers => [ UNDEF => sub {'-'} ]
  );
isa_ok($f, 'String::Print');

is($f->sprinti("price: {p EUR}#", p => 3.1415), 'price:  3.14 e#');

is($f->sprinti("count: {c}#", c => undef), 'count: -#');

#
### date-time conversion
#

$f->addModifiers( qr/T|DT|D/ =>
   sub { my ($formatter, $modif, $value, $args) = @_;
         my $time_format
           = $modif eq 'T'  ? '%T'
           : $modif eq 'D'  ? '%F'
           : $modif eq 'DT' ? '%TT%FZ'
           :                  'ERROR';
         strftime $time_format, localtime($value);
       } );

my $now = 1365850757;

is($f->sprinti("time: {t T }", t => $now), 'time: 12:59:17', 'time');
is($f->sprinti("date: {t D }", t => $now), 'date: 2013-04-13', 'date');
is($f->sprinti("both: {t DT}", t => $now), 'both: 12:59:17T2013-04-13Z', 'dateTime');
is($f->sprinti("#{t T%10s}#", t => $now), '#  12:59:17#', 'stacked');
