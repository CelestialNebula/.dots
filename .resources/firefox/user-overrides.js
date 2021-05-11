// https://github.com/arkenfox/user.js
// https://github.com/pyllyukko/user.js
/*** MY OVERRIDES ***/
user_pref("_user.js.parrot", "overrides section syntax error");
/*** [SECTION 0100]: STARTUP ***/
user_pref("browser.startup.page", 1);
user_pref("browser.startup.homepage", "about:home");
user_pref("browser.newtabpage.enabled", true); // 0104
/*** [SECTION 0800]: LOCATION BAR / SEARCH BAR / SUGGESTIONS / HISTORY / FORMS ***/
user_pref("browser.urlbar.suggest.history", false); // 0850a
user_pref("browser.urlbar.suggest.topsites", false); // [FF78+]
user_pref("browser.urlbar.autoFill", false); // 0850d
/*** [SECTION 0900]: PASSWORDS ***/
user_pref("signon.rememberSignons", false); // 0901
/*** SESSIONS & SESSION RESTORE ***/
user_pref("browser.sessionstore.interval", 300000); // 1023 (5 mins)
/*** [SECTION 1600]: HEADERS / REFERERS ***/
user_pref("network.http.sendRefererHeader", 0); // 1601 [DEFAULT: 2]
user_pref("network.http.referer.trimmingPolicy", 2); // 1602 [DEFAULT: 0]
user_pref("network.http.referer.defaultPolicy", 0); // 1606 [DEFAULT: 2 FF87+]
user_pref("network.http.referer.defaultPolicy.pbmode", 0); // 1606 [DEFAULT: 2]
/*** [SECTION 2200]: WINDOW MEDDLING & LEAKS / POPUPS ***/
user_pref("full-screen-api.enabled", false); // 2204
/*** [SECTION 2400]: DOM (DOCUMENT OBJECT MODEL) & JAVASCRIPT ***/
user_pref("dom.event.contextmenu.enabled", false); // 2401
/*** DOWNLOADS ***/
user_pref("browser.download.forbid_open_with", true); // 2654
/*** [SECTION 2700]: PERSISTENT STORAGE ***/
user_pref("network.cookie.lifetimePolicy", 2); // 2703
user_pref("dom.storageManager.enabled", false); // 2750 [FF51+]
user_pref("dom.storage_access.enabled", false); // 2755 [FF65+]
/*** [SECTION 2800]: SHUTDOWN ***/
user_pref("privacy.clearOnShutdown.siteSettings", true); // 2803 Site Preferences
user_pref("privacy.cpd.siteSettings", true); // 2804 Site Preferences
/*** [SECTION 5000]: PERSONAL ***/
// user_pref("browser.menu.showViewImageInfo", true); // [FF89] Put back View Page Info
user_pref("mousewheel.min_line_scroll_amount", 50);
user_pref("findbar.highlightAll", true);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true); // [FF68+] allow userC{hrome,ontent}.css
// user_pref("javascript.enabled", false);
user_pref("browser.bookmarks.max_backups", 2);
user_pref("browser.bookmarks.openInTabClosesMenu", false); // Keeps menu open when you click a bookmark.  ^click instead of right click
user_pref("browser.tabs.loadBookmarksInBackground", true); // Won't auto switch tabs
user_pref("browser.tabs.warnOnClose", true);
user_pref("browser.download.autohideButton", false); // [FF57+]
user_pref("browser.download.animateNotifications", false); // No animation for downloads
user_pref("browser.tabs.closeWindowWithLastTab", false);
user_pref("browser.tabs.loadBookmarksInTabs", true); // [FF57+] open bookmarks in a new tab
user_pref("browser.urlbar.decodeURLsOnCopy", true); // [FF53+] see Bugzilla 1320061
user_pref("ui.key.menuAccessKey", 0); // disable alt key toggling the menu bar [RESTART]
user_pref("network.manage-offline-status", false); // see bugzilla 620472
user_pref("extensions.pocket.enabled", false); // [FF46+] Pocket Account
user_pref("identity.fxaccounts.enabled", false); // [FF60+] Firefox Accounts and Sync [RESTART]
user_pref("_user.js.parrot", "SUCCESS");
