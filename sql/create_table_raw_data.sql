create table if not exists raw_data(
     raw_msg_id integer primary key autoincrement,
     itow integer,

     year integer,
     month integer,
     day integer,

     hour integer,
     minute integer,
     second integer,

     validity_flags integer,
     time_accuraccy integer,
     nanosecond integer,

     fix_status integer,
     fix_status_flags integer,
     date_time_flags integer,
     number_of_svs integer,

     longitude integer,
     latitude integer,

     wgs_altitude integer,
     msl_altitude integer,

     horizontal_accuracy integer,
     vertical_accuracy integer,

     speed integer,
     heading integer,

     speed_accuracy integer,
     heading_accuracy integer,

     pdop integer,

     lat_lon_flags integer,

     battery_status integer,

     g_force_x integer,
     g_force_y integer,
     g_force_z integer,

     rotation_rate_x integer,
     rotation_rate_y integer,
     rotation_rate_z integer
)
