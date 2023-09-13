create table if not exists gps_message(
  timestamp datetime,
  longitude real,
  latitude real,
  msl_altitude real,
  wgs_altitude real,
  speed real,
  heading real,
  g_force_x real,
  g_force_y real,
  g_force_z real,
  rotation_x real,
  rotation_y real,
  rotation_z real,
  raw_id integer not null,
  FOREIGN KEY (raw_id)
        REFERENCES raw_data (raw_msg_id)
             ON DELETE SET NULL
)
