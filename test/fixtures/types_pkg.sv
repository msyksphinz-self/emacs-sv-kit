package types_pkg;

  typedef enum logic [1:0] { ST_IDLE, ST_RUN, ST_DONE } state_t;

  typedef struct packed {
    logic       valid;
    logic [7:0] payload;
  } entry_t;

  function automatic int unsigned double_it(input int unsigned value);
    return value * 2;
  endfunction

endpackage
