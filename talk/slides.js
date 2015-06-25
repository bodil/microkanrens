var Pink = require("pink");
require("pink/css/themes/simon.less");
require("pink/node_modules/highlight.js/styles/googlecode.css");

new Pink("#slides", {
  "background": require("pink/modules/background"),
  "image": require("pink/modules/image"),
  "code": require("pink/modules/highlight")
});
