states <- sf::st_read(
    here::here("data/cb_2024_us_state_500k/cb_2024_us_state_500k.shp"),
    quiet = TRUE
)

islands <- do.call(
    rbind, 
    lapply(
        list(
            sf::st_read(here::here("data/islands/pw_noaa_all_shoreline.shp"),
                        quiet = TRUE) |> 
                dplyr::mutate(NAME = 'Palau',
                              STUSPS = 'PW'),
            sf::st_read(here::here("data/islands/fm_spcusgs_all_shoreline.shp"),
                        quiet = TRUE) |> 
                dplyr::mutate(NAME = 'Federated States of Micronesia',
                              STUSPS = 'FM'), 
            sf::st_read(here::here("data/islands/mh_spc_all_shoreline.shp"),
                        quiet = TRUE) |> 
                dplyr::mutate(NAME = 'Marshall Islands',
                              STUSPS = 'MH')
        ), \(x) x |> 
            dplyr::summarize(
                geometry = sf::st_union(geometry),
                NAME = head(NAME, 1),
                STUSPS = head(STUSPS, 1)
            )
    )
) |> sf::st_transform(sf::st_crs(states))