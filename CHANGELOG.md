# Version 0.3.0.9000

 * Start development version and use mungebits2 package throughout.

# Version 0.2.3

 * The `munge_procedure` passed to a `tundraContainer` can now be a stageRunner,
   in preparation for hierarchical munging in the mungebits2 package.

# Version 0.2.2

 * tundraContainer objects can now add pre-munge and post-munge hooks - functions
   that will be executed before and after munging when the `train` or
   `predict` methods are called. You can add a hook using
   `container$add_hook('train_pre_munge', function() { ... })`. Note these functions
   can modify the tundraContainer's internals, like `input` or `output`.

