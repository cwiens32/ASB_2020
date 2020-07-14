# Create database for ASB 2020 abstract
# Created: 2020 March 30
# Last run: 2020 March 30

library(RSQLite)
library(tidyverse)
library(plotly)


# database ----------------------------------------------------------------

# connect to database
dbOG <- 'C:\\Users\\cwiens\\Box Sync\\USC - Biomechanics Lab\\Conferences\\ASB2020\\ASB2020_Database.sqlite'
connOG <- dbConnect(SQLite(), dbOG)
db <- 'C:\\Users\\cwiens\\Documents\\R\\Conferences\\2020 ASB\\ASB2020_Database.sqlite'
conn <- dbConnect(SQLite(), db)


# create data table -------------------------------------------------------

# load tables
ls = dbReadTable(connOG, 'logsheet') %>%
  select('collection_id', 'trial_id', 'athlete_id', 'shot_type', 'distance', 'result')
# load sensor data
sensor.data = dbReadTable(connOG, 'sensor_results') %>% 
  select(-"angle_shotinit_trunk", -"angle_release_trunk") %>% 
  mutate(trial_id = as.double(trial_id))
# load ball data
ball.var = dbReadTable(connOG, 'ballvar') %>%
  mutate(Trial_ID = as.double(Trial_ID))
names(ball.var) <- tolower(names(ball.var))
# load force data
shotimp = dbReadTable(connOG, 'shotimp') %>% 
  select('Trial_ID', 'velD_Y_release', 'velD_Z_release') %>% 
  mutate(Trial_ID = as.double(Trial_ID))
names(shotimp) <- tolower(names(shotimp))
# combine tables
sensortab = merge(sensor.data, ls, by='trial_id') %>% 
  mutate(athlete_id = factor(athlete_id),
         distance = factor(distance, levels = c(6,15,19.75)))
balltab = merge(ball.var, ls)
data = merge(sensortab,balltab)
data = merge(data, shotimp)
# write table to new database
dbWriteTable(conn, "data", data, overwrite = TRUE)

# load table of tables
taball <- merge(merge(dbGetQuery(connOG, 'SELECT * FROM table_info'),
                      dbGetQuery(connOG, 'SELECT * FROM logsheet') %>% 
                        select('trial_id', 'athlete_id', 'distance',
                               'result', 'force_file')),
                dbReadTable(connOG, "sync_table") %>% 
                  select(-"collection_id")) %>% 
  mutate(release_sensor = release_sensor * (1/120),
         shotinit_sensorarm = shotinit_sensorarm * (1/120))
# write table to new database
dbWriteTable(conn, "table_info", taball)


# load acceleration data tables into master table -------------------------

# load acceleration tables
for (cnt in 1:dim(taball)[1]){
  if (!is.na(taball$acc[cnt])){
    if (exists("accall")){
      accall <- rbind(accall,
                        dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$acc[cnt])) %>% 
                          filter(time >= taball$shotinit_sensorarm[cnt] & time <= taball$release_sensor[cnt]) %>%
                          mutate(athlete_id = taball$athlete_id[cnt],
                                 trial_id = taball$trial_id[cnt],
                                 time = time - taball$release_sensor[cnt],
                                 shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
                                 distance = factor(taball$distance[cnt]),
                                 result = taball$result[cnt]))
    }else{
      # load first table
      accall <- dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$acc[cnt])) %>% 
        filter(time >= taball$shotinit_sensorarm[cnt] & time <= taball$release_sensor[cnt]) %>% 
        mutate(athlete_id = taball$athlete_id[cnt],
               trial_id = taball$trial_id[cnt],
               time = time - taball$release_sensor[cnt],
               shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
               distance = factor(taball$distance[cnt]),
               result = taball$result[cnt])
    }
  }
}
# write table to new database
dbWriteTable(conn, "accdata", accall)


# load angle data tables into master table --------------------------------

# load angle tables
for (cnt in 1:dim(taball)[1]){
  if (!is.na(taball$angle[cnt])){
    if (exists("angleall")){
      angleall <- rbind(angleall,
                        dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$angle[cnt])) %>% 
                          filter(time >= taball$shotinit_sensorarm[cnt] & time <= taball$release_sensor[cnt]) %>%
                          mutate(athlete_id = taball$athlete_id[cnt],
                                 trial_id = taball$trial_id[cnt],
                                 time = time - taball$release_sensor[cnt],
                                 shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
                                 distance = factor(taball$distance[cnt]),
                                 result = taball$result[cnt]))
    }else{
      # load first table
      angleall <- dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$angle[cnt])) %>% 
        filter(time >= taball$shotinit_sensorarm[cnt] & time <= taball$release_sensor[cnt]) %>% 
        mutate(athlete_id = taball$athlete_id[cnt],
               trial_id = taball$trial_id[cnt],
               time = time - taball$release_sensor[cnt],
               shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
               distance = factor(taball$distance[cnt]),
               result = taball$result[cnt])
    }
  }
}
# write table to new database
dbWriteTable(conn, "angledata", angleall)


# load angular velocity data tables into master table ---------------------

# load angular velocity tables
for (cnt in 1:dim(taball)[1]){
  if (!is.na(taball$angvel_world[cnt])){
    if (exists("angvelall")){
      angvelall <- rbind(angvelall,
                        dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$angvel_world[cnt])) %>%
                          filter(time >= taball$shotinit_sensorarm[cnt] & time <= taball$release_sensor[cnt]) %>% 
                          mutate(athlete_id = taball$athlete_id[cnt],
                                 trial_id = taball$trial_id[cnt],
                                 time = time - taball$release_sensor[cnt],
                                 shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
                                 distance = factor(taball$distance[cnt]),
                                 result = taball$result[cnt]))
    }else{
      # load first table
      angvelall <- dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$angvel_world[cnt])) %>% 
        filter(time >= taball$shotinit_sensorarm[cnt] & time <= taball$release_sensor[cnt]) %>% 
        mutate(athlete_id = taball$athlete_id[cnt],
               trial_id = taball$trial_id[cnt],
               time = time - taball$release_sensor[cnt],
               shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
               distance = factor(taball$distance[cnt]),
               result = taball$result[cnt])
    }
  }
}
# write table to new database
dbWriteTable(conn, "angveldata", angvelall)


# load force data tables into master table --------------------------------

# load force tables
for (cnt in 1:dim(taball)[1]){
  if (!is.na(taball$force[cnt])){
    if (exists("forceall")){
      forceall <- rbind(forceall,
                        dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$force[cnt])) %>% 
                          filter(Time >= taball$shotinit_sensorarm[cnt] & Time <= taball$release_sensor[cnt]) %>%
                          mutate(athlete_id = taball$athlete_id[cnt],
                                 trial_id = taball$trial_id[cnt],
                                 Time = Time - taball$release_sensor[cnt],
                                 shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
                                 distance = factor(taball$distance[cnt]),
                                 result = taball$result[cnt]))
    }else{
      # load first table
      forceall <- dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$force[cnt])) %>% 
        filter(Time >= taball$shotinit_sensorarm[cnt] & Time <= taball$release_sensor[cnt]) %>% 
        mutate(athlete_id = taball$athlete_id[cnt],
               trial_id = taball$trial_id[cnt],
               Time = Time - taball$release_sensor[cnt],
               shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
               distance = factor(taball$distance[cnt]),
               result = taball$result[cnt])
    }
  }
}
# change variables to lowercase
names(forceall) <- tolower(names(forceall))
# write table to new database
dbWriteTable(conn, "forcedata", forceall, overwrite = TRUE)


# load cm kinematic data tables into master table ---------------------------

# load cm kinematic tables
for (cnt in 1:dim(taball)[1]){
  if (!is.na(taball$cmkinem[cnt])){
    if (exists("cmall")){
      cmall <- rbind(cmall,
                      dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$cmkinem[cnt])) %>% 
                        filter(Time >= taball$shotinit_sensorarm[cnt] & Time <= taball$release_sensor[cnt]) %>%
                        mutate(athlete_id = taball$athlete_id[cnt],
                               trial_id = taball$trial_id[cnt],
                               Time = Time - taball$release_sensor[cnt],
                               shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
                               distance = factor(taball$distance[cnt]),
                               result = taball$result[cnt]))
    }else{
      # load first table
      cmall <- dbGetQuery(connOG, sprintf('SELECT * FROM %s', taball$cmkinem[cnt])) %>% 
        filter(Time >= taball$shotinit_sensorarm[cnt] & Time <= taball$release_sensor[cnt]) %>% 
        mutate(athlete_id = taball$athlete_id[cnt],
               trial_id = taball$trial_id[cnt],
               Time = Time - taball$release_sensor[cnt],
               shotinit = taball$shotinit_sensorarm[cnt] - taball$release_sensor[cnt],
               distance = factor(taball$distance[cnt]),
               result = taball$result[cnt])
    }
  }
}
# change variables to lowercase
names(cmall) <- tolower(names(cmall))
# write table to new database
dbWriteTable(conn, "cmkinemdata", cmall)


# disconnect from databases
dbDisconnect(connOG)
dbDisconnect(conn)



# plot angle angle --------------------------------------------------------

ggplot(angleall) +
  geom_point(aes(x = lowerarm, y = upperarm, color = distance)) +
  facet_wrap(angleall$athlete_id) +
  xlab("Upper Arm Angle (deg)") +
  ylab("Lower Arm Angle (deg)") +
  ggtitle("Arm Angle-Angle")

# plot angle vs time ------------------------------------------------------

ggplot(angleall) +
  geom_point(aes(x = time, y = lowerarm, color = distance), shape = 22) +
  geom_point(aes(x = time, y = upperarm, color = distance), shape = 25) +
  facet_wrap(angleall$athlete_id) +
  geom_hline(aes(yintercept = 0)) +
  xlab("Time (s)") +
  ylab("Angle (deg)") +
  ggtitle("Arm Angle vs Time")

# plot angular velocity vs time -------------------------------------------

ggplot(angvelall) +
  geom_point(aes(x = time, y = lowerarm_y, color = distance), shape = 22) +
  geom_point(aes(x = time, y = upperarm_y, color = distance), shape = 25) +
  facet_wrap(angleall$athlete_id) +
  geom_hline(aes(yintercept = 0)) +
  xlab("Time (s)") +
  ylab("Angular Velocity (deg/s)") +
  ggtitle("Arm Angular Velocity vs Time")

# force vs time -----------------------------------------------------------

ggplot(forceall) +
  geom_point(aes(x = time, y = z, color = distance)) +
  facet_wrap(forceall$athlete_id) +
  xlab("Time (s)") +
  ylab("Force (N)") +
  ggtitle("Force Time Curves")
