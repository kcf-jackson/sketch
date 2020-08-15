document.addEventListener("keyup", function(e) {
  console.log(e);
  if (e.key === "Enter" && (e.altKey || e.metaKey || e.ctrlKey)) {
    e.preventDefault();
    $("#transpile").click();
  }
});

Shiny.addCustomMessageHandler("switch_example",
  function(message) {
      console.log(message);
      $('textarea#src').val(message);
  }
);

Shiny.addCustomMessageHandler("toggle_engine",
  function(message) {
      $('iframe#tgt-render').toggle();
      $('textarea#console').toggle();
  }
);

Shiny.addCustomMessageHandler("replace_textarea",
  function(message) {
      console.log(message);
      $('textarea#tgt').val(message.transpiled);
      if (message.tgt == "Browser") {
        $('iframe#tgt-render').attr("src", message.path);
      } else { // V8
        $('textarea#console').val(message.rendered);
      }
  }
);
