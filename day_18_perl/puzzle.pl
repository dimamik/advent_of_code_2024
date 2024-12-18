use strict;
use warnings;
use Graph;

sub read_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die $!;
    my @lines;
    while (<$fh>) {
        chomp;
        push @lines, [map { int($_) } split /,/];
    }
    close $fh;
    return @lines;
}

sub initialize_graph {
    my ($rows, $cols) = @_;
    my $G = Graph->new(undirected => 1);
    $G->add_vertices(0 .. ($rows*$cols - 1));
    for my $r (0 .. $rows-1) {
        for my $c (0 .. $cols-1) {
            my $n = $r * $cols + $c;
            $G->add_edge($n, ($r+1)*$cols+$c) if $r+1 < $rows;
            $G->add_edge($n, $r*$cols+($c+1)) if $c+1 < $cols;
            $G->add_edge($n, ($r-1)*$cols+$c) if $r-1 >= 0;
            $G->add_edge($n, $r*$cols+($c-1)) if $c-1 >= 0;
        }
    }
    return $G;
}

sub remove_nodes {
    my ($G, $list, $idx, $cols) = @_;
    for my $i (0 .. $idx-1) {
        my ($r, $c) = @{$list->[$i]};
        my $n = $r * $cols + $c;
        $G->delete_vertex($n) if $G->has_vertex($n);
    }
    return $G;
}

sub has_path {
    my ($G, $start, $end) = @_;
    my @q = ($start);
    my %v = ($start => 1);
    while (@q) {
        my $cur = shift @q;
        return 1 if $cur == $end;
        for my $w ($G->successors($cur)) {
            next if $v{$w};
            $v{$w} = 1;
            push @q, $w;
        }
    }
    return 0;
}

sub shortest_path_length {
    my ($G, $start, $end) = @_;
    my @q = ([$start,0]);
    my %v = ($start => 1);
    while (@q) {
        my ($cur, $d) = @{shift @q};
        return $d if $cur == $end;
        for my $w ($G->successors($cur)) {
            next if $v{$w};
            $v{$w} = 1;
            push @q, [$w, $d+1];
        }
    }
    return -1;
}

sub part_1 {
    my ($rows, $cols, $obstacles) = @_;
    my $G = initialize_graph($rows, $cols);
    remove_nodes($G, $obstacles, 1024, $cols);
    return shortest_path_length($G, 0, $rows*$cols-1);
}

sub part_2 {
    my ($rows, $cols, $obstacles) = @_;
    my $left = 0;
    my $right = scalar(@$obstacles);
    while ($right - $left > 1) {
        my $mid = int(($right + $left) / 2);
        my $G = initialize_graph($rows, $cols);
        remove_nodes($G, $obstacles, $mid, $cols);
        if (has_path($G, 0, $rows*$cols-1)) {
            $left = $mid;
        } else {
            $right = $mid;
        }
    }
    return "$obstacles->[$left][0],$obstacles->[$left][1]";
}

my ($rows, $cols) = (71, 71);
my @lines = read_input('input.txt');
print part_1($rows, $cols, \@lines), "\n";
print part_2($rows, $cols, \@lines), "\n";
