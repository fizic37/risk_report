function plafoane_module_js(ns_prefix) {
  
  $("#" + ns_prefix + "baza_surse_proprii").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "data_raport_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  }); 
  
  $("#" + ns_prefix + "baza_surse_proprii").on("click", ".download_btn", function() {
    Shiny.setInputValue(ns_prefix + "data_raport_to_download", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
}



