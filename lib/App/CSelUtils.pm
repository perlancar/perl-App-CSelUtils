package App::CSelUtils;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

our %SPEC;

# arguments for utilities like orgsel, htmlsel

our %foosel_common_args = (
    expr => {
        schema => 'str*',
        req => 1,
        pos => 0,
    },
    file => {
        schema => 'str*',
        'x.schema.entity' => 'filename',
        pos => 1,
        default => '-',
    },
);

our %foosel_struct_action_args = (
    actions => {
        summary => 'Specify action(s) to perform on matching nodes',
        'x.name.is_plural' => 1,
        schema => ['array*', {
            of => ['str*', {
                in => ['print', 'count'],
            }],
        }],
        default => ['print'],
        cmdline_aliases => {
            print => {
                summary => 'Shortcut for --action print',
                is_flag => 1,
                code => sub {
                    my $args = shift;
                    $args->{actions} //= [];
                    my $actions = $args->{actions};
                    unless (grep {$_ eq 'print'} @$actions) {
                        push @$actions, 'print';
                    }
                },
            },
            count => {
                summary => 'Shortcut for --action count',
                is_flag => 1,
                code => sub {
                    my $args = shift;
                    $args->{actions} //= [];
                    my $actions = $args->{actions};
                    unless (grep {$_ eq 'count'} @$actions) {
                        push @$actions, 'count';
                    }
                },
            },
        },
    },
);

our %foosel_tree_action_args = (
    actions => {
        summary => 'Specify action(s) to perform on matching nodes',
        'x.name.is_plural' => 1,
        schema => ['array*', {
            of => ['str*', {
                match => qr/\A(dump|print_as_string|print_method:\w+(\.\w+)*|count)\z/,
            }],
        }],
        default => ['print_as_string'],
        cmdline_aliases => {
            print => {
                summary => 'Shortcut for --action print_as_string',
                is_flag => 1,
                code => sub {
                    my $args = shift;
                    $args->{actions} //= [];
                    my $actions = $args->{actions};
                    unless (grep {$_ eq 'print_as_string'} @$actions) {
                        push @$actions, 'print_as_string';
                    }
                },
            },
            count => {
                summary => 'Shortcut for --action count',
                is_flag => 1,
                code => sub {
                    my $args = shift;
                    $args->{actions} //= [];
                    my $actions = $args->{actions};
                    unless (grep {$_ eq 'count'} @$actions) {
                        push @$actions, 'count';
                    }
                },
            },
            dump => {
                summary => 'Shortcut for --action dump',
                is_flag => 1,
                code => sub {
                    my $args = shift;
                    $args->{actions} //= [];
                    my $actions = $args->{actions};
                    unless (grep {$_ eq 'dump'} @$actions) {
                        push @$actions, 'dump';
                    }
                },
            },
            print_method => {
                summary => '--print-method M is shortcut for --action print_method:M',
                code => sub {
                    my ($args, $val) = @_;
                    $args->{actions} //= [];
                    my $actions = $args->{actions};
                    push @$actions, "print_method:$val";
                },
            },
        },
    },
);

$SPEC{parse_csel} = {
    v => 1.1,
    summary => 'Parse CSel expression',
    args => {
        expr => {
            schema => 'str*',
            req => 1,
            pos => 0,
        },
    },
    'cmdline.default_format' => 'json-pretty',
};
sub parse_csel {
    require Data::CSel;
    my %args = @_;
    [200, "OK", Data::CSel::parse_csel($args{expr})];
}

# routines for utilities like orgsel, htmlsel
sub do_actions_on_nodes {
    my %args = @_;

    my $nodes = $args{nodes};
    my $actions = $args{actions};

    my $res = [200, "OK"];
    for my $action (@$actions) {
        if ($action eq 'dump') {
            require Tree::ToTextLines;
            push @{ $res->[2] }, map {
                Tree::ToTextLines::render_tree_as_text({
                    show_guideline  => 1,
                    show_class_name => 1,
                    id_attribute    => "id",
                }, $_)
              } @$nodes;
        } elsif ($action eq 'count') {
            if (@$actions == 1) {
                $res->[2] = ~~@$nodes;
            } else {
                push @{ $res->[2] }, ~~@$nodes;
            }
        } elsif ($action eq 'print_as_string') {
            push @{ $res->[2] }, map {$_->as_string} @$nodes;
        } elsif ($action =~ /\Aprint_method:(.+)\z/) {
            my @meths = split /\./, $1;
            for my $node (@$nodes) {
                my $node_res = $node;
                for my $meth (@meths) {
                    eval { $node_res = $node_res->$meth };
                    if ($@) {
                        $node_res = undef;
                        last;
                    }
                }
                push @{ $res->[2] }, $node_res;
            }
        } elsif ($action =~ /\Aprint_func:(.+)\z/) {
            no strict 'refs';
            my @funcs = split /\./, $1;
            for my $node (@$nodes) {
                my $node_res = $node;
                for my $func (@funcs) {
                    eval { $node_res = &{$func}($node_res) };
                    if ($@) {
                        $node_res = undef;
                        last;
                    }
                }
                push @{ $res->[2] }, $node_res;
            }
        } elsif ($action =~ /\Aprint_func_or_meth:(.+)\z/) {
            no strict 'refs';
            my @entries = split /\./, $1;
            for my $node (@$nodes) {
                my $node_res = $node;
                for my $entry (@entries) {
                    my ($type, $name) = $entry =~ /\A(func|meth)::?(.+)\z/ or
                        return [400, "For action print_func_or_meth, ".
                                    "specify func:FUNCNAME or meth:METHNAME"];
                    eval {
                        if ($type eq 'func') {
                            #use DD; say "func: $name(", DD::dump($node_res), ")";
                            $node_res = &{$name}($node_res);
                        } else {
                            #use DD; say "meth: $name on ", DD::dump($node_res);
                            $node_res = $node_res->$name;
                        }
                    };
                    if ($@) {
                        #warn $@;
                        $node_res = undef;
                        last;
                    }
                }
                push @{ $res->[2] }, $node_res;
            }
        } else {
            return [400, "Unknown action '$action'"];
        }
    }
    $res;
}

$SPEC{ddsel} = {
    v => 1.1,
    summary => 'Select Perl data structure elements using CSel (CSS-selector-like) syntax',
    description => <<'_',

Note that this operates against Perl data structure, not Perl source code
elements (see <prog:ppisel> for that). File is Perl source code that defines
data structure, e.g.:

    {
        summary => 'This is a hash',
        # this is an array inside a hash
        array => [
            1, 2, 3,
        ],
    };

_
    args => {
        %foosel_common_args,
        %foosel_struct_action_args,
    },
};
sub ddsel {
    my %args = @_;

    my $expr = $args{expr};
    my $actions = $args{actions};

    # parse first so we can bail early on error without having to read the input
    require Data::CSel;
    Data::CSel::parse_csel($expr)
          or return [400, "Invalid CSel expression '$expr'"];

    my $data;
    if ($args{file} eq '-') {
        binmode STDIN, ":encoding(utf8)";
        $data = eval join("", <>);
        die if $@;
    } else {
        require File::Slurper;
        $data = eval File::Slurper::read_text($args{file});
        die if $@;
    }

    require Data::CSel::WrapStruct;
    my $tree = Data::CSel::WrapStruct::wrap_struct($data);

    my @matches = Data::CSel::csel(
        {class_prefixes=>['Data::CSel::WrapStruct']}, $expr, $tree);

    # skip root node itself
    require Scalar::Util;
    @matches = grep {
        Scalar::Util::refaddr($_) ne Scalar::Util::refaddr($tree) } @matches
              unless @matches <= 1;

    for my $action (@$actions) {
        if ($action eq 'print') {
            $action = 'print_func_or_meth:meth:value.func:Data::Dmp::dmp';
        }
    }

    require Data::Dmp;
    App::CSelUtils::do_actions_on_nodes(
        nodes   => \@matches,
        actions => $args{actions},
    );
}

1;

# ABSTRACT: Utilities related to Data::CSel

=for Pod::Coverage ^(do_actions_on_nodes)$

=head1 DESCRIPTION

This distribution contains the following utilities:

# INSERT_EXECS_LIST


=head1 SEE ALSO

L<htmlsel>, L<orgsel>, L<jsonsel>, L<yamlsel>, L<podsel>, L<ppisel>

=cut
