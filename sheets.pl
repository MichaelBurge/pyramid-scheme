#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

# There are two types of lines: 
# * 20c:0: label0x20b: Push1 0x1;
# * 1f6:0: Push2 0x20b
# 
while (<>) {
    chomp;
    $_ =~ s/;//g;
    my ($offset, undef, $op_or_label, $op_or_null) = split /:/;
    my $op;
    my $label;
    # Type 1
    if ($op_or_null) {
        $label = $op_or_label;
        $op = $op_or_null;
    # Type 2
    } else {
        $label = undef;
        $op = $op_or_label;
    }
    my $cols = [
        $offset,
        $label || '',
        $op
        ];
    say(join("\t", @$cols));
}
