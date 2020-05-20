########################
# RANDOM NUMBER SCRIPT #
########################

# Because the younger networks continually
# produced equal, mid-range (~0.5) output activations for the consistent
# and inconsistent test events, I used a random number generator to code
# the younger network's test choices. Thus, a network was said to have
# made a correct test choice if the random-number generator produced a '1'.
# Otherwise, the network was said to have made an incorrect test choice and,
# hence, assigned a '0'.

set.seed(2245)
rand_younger_net = sample(c(0,1),20, replace = TRUE)
rand_younger_net
table(rand_younger_net)

# additional nets
rand_younger_net_additional = sample(c(0,1),12, replace = TRUE)
rand_younger_net_additional
table(rand_younger_net_additional)