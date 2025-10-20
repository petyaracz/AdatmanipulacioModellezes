d0 = read_tsv('https://raw.githubusercontent.com/petyaracz/RaczLukacs2024/refs/heads/main/tidy/d.tsv')

d1 = d0 |> 
  filter(
    !drop_participant,
    !drop_observation,
    !nonce_word,
    participant_age > 17, participant_age < 36
  )

d2a = d1 |> 
     count(
         correct,word,lfpm10r
         ) |> 
     pivot_wider(names_from = correct, values_from = n, values_fill = 0) |> 
     rename(
         n_known = `TRUE`,
         n_unknown = `FALSE`
       ) |> 
     mutate(
         p_known = n_known / (n_known + n_unknown),
         odds_known = (n_known + 1) / (n_unknown + 1),
         log_odds_known = log(odds_known)
       )

d2b = d1 |> 
  summarise(mean_log_rt = mean(log(resp.rt)), .by = word)

d2 = left_join(d2a,d2b)

d3 = d1 |> 
  mutate(
    knows = correct,
    log_rt = log(resp.rt)
  ) |> 
  select(knows,word,id,lfpm10r,log_rt)

d2 |> write_tsv('~/Github/AdatmanipulacioModellezes/dat/R2/l4d1.tsv')
d3 |> write_tsv('~/Github/AdatmanipulacioModellezes/dat/R2/l4d2.tsv')
