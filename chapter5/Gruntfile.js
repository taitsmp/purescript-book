module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],

    psc: {
      options: {
        modules: ["Data.Picture", "C5ex"]
      },
      all: {
      src: ["<%=srcFiles%>"],
        dest: "dist/Main.js"
      }
    },

    dotPsci: ["<%=srcFiles%>"]
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["psc:all", "dotPsci"]);
};
