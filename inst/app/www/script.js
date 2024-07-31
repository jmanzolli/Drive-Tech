Shiny.addCustomMessageHandler("bttn_active", function (message) {
    // Remove the active class from the buttons to deactivate
    message.desactive.forEach(function(buttonId) {
        $("#" + buttonId).removeClass("active");
    });
    
    // Add the active class to the active button
    $("#" + message.active).addClass("active");
});
