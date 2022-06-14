{ config, pkgs, ... }:

{
  # Overwrite desktop entry for firefox, just so we can add the 'nixGLIntel' wrapper
  xdg.desktopEntries.firefox = {
    categories = ["Network" "WebBrowser"];
    exec = "nixGLIntel firefox %U";
    genericName = "Web Browser";
    icon = "firefox";
    mimeType = ["text/html" "text/xml" "application/xhtml+xml" "application/vnd.mozilla.xul+xml" "x-scheme-handler/http" "x-scheme-handler/https" "x-scheme-handler/ftp"];
    name = "Firefox";
    type = "Application";
  };

  programs.firefox = {
    enable = true;

    # package =
    #   pkgs.writeShellScriptBin "firefox" ''
    #     #!/bin/sh
    #     ${pkgs.nixGL}/bin/nixGLIntel ${pkgs.firefox}/bin/firefox "$@"
    #   '';

    profiles = {
      "${config.home.username}" = {

        # These settings end up in user.js, and subsequently get copied to prefs.js
        # You can check out what else has been configured by looking at about:config
        settings = {
          # Proxy settings using firefox.pac file configured elsewhere
          "network.proxy.autoconfig_url" = "file://${config.xdg.configHome}/firefox.pac";
          "network.proxy.socks_remote_dns" = true;
          "network.proxy.type" = 2;

          # Enable userChrome and userContent files
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

          # Force enable WegGL, this still only works if you run firefox with the nixGL wrapper
          # https://get.webgl.org/
          # https://onxmapssupport.zendesk.com/hc/en-us/articles/4414111261581-Web-Browser-does-not-support-MapBoxGL
          # https://support.mozilla.org/en-US/questions/1347475
          # Disable 'Recommended performance settings'
          "browser.preferences.defaultPerformanceSettings.enabled" = false;
          "webgl.disabled" = false;
          "webgl.force-enabled" = true;
        };

        # https://www.userchrome.org/adding-style-recipes-userchrome-css.html
        userChrome = ''
@-moz-document url(chrome://browser/content/browser.xhtml) {
  /* hide icons for bookmark toolbar */
  #personal-bookmarks .bookmark-item > .toolbarbutton-icon {
      display:none!important;
  }
}
'';

        userContent = ''
@-moz-document domain(app.channable.com) {
  .top-bar, .top-bar-row {
    background-color: #ff6666 !important;
  }
  .progress-loader svg path.meter {
      transition: stroke 0.05s ease-in-out !important;
  }
  .progress-loader svg.easing path.meter {
      transition: stroke-dashoffset 0.1s linear, stroke 0.1s ease-in-out !important;
  }
  .home .project {
      width: 200px !important;
  }
}

@-moz-document domain(localhost) {
  .top-bar, .top-bar-row {
    background-color: #88ee88 !important;
  }
  .progress-loader svg path.meter {
      transition: stroke 0.05s ease-in-out !important;
  }
  .progress-loader svg.easing path.meter {
      transition: stroke-dashoffset 0.1s linear, stroke 0.1s ease-in-out !important;
  }
  .home .project {
      width: 200px !important;
  }
}

@-moz-document domain(trello.com) {
  #classic-body {
    background-color: #456789;
    background-image: none !important;
  }
}
'';
      };
    };
  };

  # Old proxy tunneling style for grafana.query.consul
#   xdg.configFile = {
#     "firefox.pac" = {
#       text = ''
# // Necessary for grafana
# function FindProxyForURL(url, host) {
#   if (host.endsWith('.consul')) {
#     return 'SOCKS localhost:4999';
#   } else {
#     return 'DIRECT';
#   }
# }'';
#     };
#   };
}
