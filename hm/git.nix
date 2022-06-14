{ config, pkgs, ... }:

{
  # This git configuration is put under `.config/git/config`, but
  # it still reads `.gitconfig` as well. T
  programs.git = {
    enable = true;
    lfs.enable = true;
    extraConfig = {

      #core.editor = emacsclient;

      core.excludesfile = "${config.home.homeDirectory}/.gitignore_global";

      merge.conflictstyle = "diff3";
      # Prevent bad objects from spreading.
      transfer.fsckObjects = true;
      diff.algorithm = "histogram";
      # Try to break up diffs at blank lines
      diff.compactionHeuristic = true;
      # Include tags with commits that we push
      push.followTags = true;
      # Sort tags in version order, e.g. `v1 v2 .. v9 v10` instead
      # of `v1 v10 .. v9`
      tag.sort = "version:refname";
      # For interactive rebases, automatically reorder and set the
      # right actions for !fixup and !squash commits.
      rebase.autosquash = true;

      # Remeber conflict resolutions. If the same conflict appears
      # again, use the previous resolution.
      rerere.enabled = true;
      # Stage solved conflicts automatically
      # rerere.autoupdate = true
    };
  };

  home.file = {
    ".gitignore_global" = {
      text = ''
TAGS
*.dump-splices
.projectile
.#*
'';
    };
  };
}
