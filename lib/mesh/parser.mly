%{
  open Easy_logging

  let logger = Logging.make_logger "Parser" Info [Cli Debug]
%}

%token PLACEHOLDER
%token EOF

%start <unit> source_unit
%%

source_unit:
  | PLACEHOLDER; source_unit                                                    { () }
  | EOF                                                                         { logger#debug "EOF"; () }
;