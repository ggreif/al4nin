My code is migrated from http://developer.berlios.de/projects/al4nin/
to this place. BerliOS shuts down end of this year.

# The original summary on BerliOS #

Just like alanine is an essential amino acid needed for protein synthesis, a garbage collector is needed for every modern operating system. AL4nin tries to be that for an L4 based Dylan OS. See design document at http://www.gwydiondylan.org/papers/

## Migrating WCs already checked out ##

Do this:
```
cd <my-wc>
svn switch --relocate https://svn.berlios.de/svnroot/repos/al4nin/trunk https://al4nin.googlecode.com/svn/trunk .
```
You'll get an error message like this:
_svn: The repository at 'https://al4nin.googlecode.com/svn/trunk' has uuid 'a318f3ce-71e3-817b-b164-db38ee571ebd', but the WC has '1c9cc00f-83e4-0310-8ba3-b4ab0ce7b12c'_

On Linux it is easy to fix this up:
```
find . -type f -name entries -exec sed -i 's/OLD-uuid/NEW-uuid/g' {} \;
```

For svn-1.7 (and up) consult [stack overflow](http://superuser.com/questions/229983/how-to-change-subversion-working-copy-uuid) for a recipe.