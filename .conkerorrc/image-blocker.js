require("content-policy.js");

content_policy_bytype_table.image = function () content_policy_reject;
add_hook("content_policy_hook", content_policy_bytype);