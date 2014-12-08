# Version 0.2.2

 * tundraContainer objects can now add pre-munge and post-munge hooks - functions
   that will be executed before and after munging when the `train` or
   `predict` methods are called. You can add a hook using
   `container$add_hook('train_pre_munge', function() { ... })`. Note these functions
   can modify the tundraContainer's internals, like `input` or `output`.

