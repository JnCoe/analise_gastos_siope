# Rascunho de coisas que vão entrar depois:

Vendo quem é que tem esse valor de salário discrepante (acima de 100 mil):
  
  ```{r}

rem_prof_sp %>%
  filter(VL_SALARIO > 100000) %>%
  select(NO_UF, NO_MUNICIPIO, NO_PROFISSIONAL, CATEG_PROFISSIONAL, VL_SALARIO) %>%
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"))

```


###3. Valores de salários menores que R$ 1
```{r}
rem_prof_sp %>%
  mutate(sem_info_salario = ifelse(VL_SALARIO < 1, 1, 0)) %>%
  summarise(total_sem_info_salario = sum(sem_info_salario))
```
<br>
  Verificamos que 24 salários constam abaixo de 1 real, vou ver se é o caso de alguem  



