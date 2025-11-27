#!/usr/bin/env perl
# Test the chop modifier

use warnings;
use strict;
use utf8;

use Test::More;

use String::Print;

my $f = String::Print->new;
isa_ok($f, 'String::Print');

my $text1 = "123456789012345678901234567890";
cmp_ok length($text1), '==', 30, 'check text length';

is $f->sprinti("Intro: {text}", text => $text1), "Intro: $text1", 'no modifier';

is $f->sprinti("Intro: {text CHOP(50)}", text => $text1), "Intro: $text1", 'fits in field easily';

### these are all examples from the manual page

is $f->sprinti("Intro: {text CHOP(25)}", text => $text1),
	"Intro: 123456789012345678901[+9]", 'chop short';

is $f->sprinti("Intro: {text CHOP(10)}", text => $text1),
	"Intro: 12345[+25]", 'chop short';

#XXX this needs testing for wide and zero-width strings

done_testing;
