{ pkgs, ... }:
let
  cmd-shell = contents: [ "CMD-SHELL" contents ];
in
{
  config.project.name = "webapp";
  config.services = {
    webserver = {
      service.useHostStore = true;
      service.command = "my-sample";
      service.ports = [
        # not neccessary for test
        "3000:3000"
      ];
      service.stop_signal = "SIGINT";
      service.healthcheck = {
        interval = "1s";
        retries = 3;
        timeout = "10s";
        test = cmd-shell ''
          curl -f http://localhost:3000/healthcheck
        '';
      };
      image.contents = with pkgs; [
        curl
        my-sample
        # for debugging
        bashInteractive
      ];
    };

    test = {
      service.useHostStore = true;
      service.depends_on = {
        webserver.condition = "service_healthy";
      };
      service.command = [ "bash" "${./test/integration/test.bash}" ];
      service.environment = {
        "HOST" = "webserver:3000";
      };
      image.contents = with pkgs; [
        bashInteractive
        stdenv
        coreutils
        curl
        gnugrep
      ];
    };
  };
}


