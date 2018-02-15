## Settings
Init
~~~bash
cabal configure --enable-tests --enable-benchmarks
cabal install --enable-tests --enable-benchmarks
cabal build
cabal check
hlint .
cabal sdist
~~~

Reload docs
~~~bash
cabal hscolour
cabal haddock --hyperlink-source
~~~

And upload to git
~~~
git tag -a vX.X COMMIT
git push origin --tags
~~~

Test
~~~bash
cabal test --show-details=always
cabal test SUITE --show-details=always
cabal bench
~~~
