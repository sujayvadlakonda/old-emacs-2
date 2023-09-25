When you push, do so on `origin` with `git push origin`.
When you want to pull changes from `purcell` you can just fetch the remote and rebase on top of your work.
```bash
git fetch purcell
git rebase purcell/master
```
And solve the conflicts if any
