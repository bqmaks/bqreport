# Контекст для следующей сессии

Обновлено: 2026-08-13 (Europe/Moscow).

## Изменения сессии 2026-08-13 (вечер)

Пакет прошёл внешнюю ревизию; все найденные проблемы исправлены:

- **Баг resampling**: `resampled_correlation()` теперь ресемплирует веса
  вместе с наблюдениями; для методов с subject id — cluster bootstrap по
  субъектам и перестановки только внутри субъекта
  (`R/correlation-methods.R`: `resample_correlation_context()`,
  `permute_correlation_context()`).
- **Spearman CI**: SE Bonett–Wright `sqrt((1 + r^2/2)/(n - k - 3))`,
  `ci_method = "fisher_z_bonett_wright"`.
- **Point-estimate-only методы** (`biweight_correlation()`): статус
  `review` при валидации; запуск после `approve_plan()` или через
  `resampled_correlation()`. Ревизия учитывает `approved` при
  повторной валидации внутри `run_analysis()`.
- **Минимум наблюдений** для resampled-методов берётся от base method.
- **Детерминированные id**: все идентификаторы — digest содержимого
  (`R/ids.R`, `bq_id()`); `uuid` удалён из Imports. Одинаковый вход даёт
  идентичные планы/результаты между запусками
  (`tests/testthat/test-reproducible-ids.R`). `var_id` = `var_<имя>` с
  детерминированной суффиксацией при повторном использовании имени.
- **Файлы разрезаны**: `correlations.R` → `correlation-methods/plan/
  execute/interactions.R`; `analysis-result.R` → `+ engines-builtin.R`,
  `engines-custom.R`, `result-components.R`, `result-accessors.R`.
- Примеры `@examples` у ключевых экспортов; `NEWS.md`; реальный автор в
  `DESCRIPTION`; workflow `.github/workflows/R-CMD-check.yaml`;
  AGENTS.md синхронизирован (Imports, стиль ошибок, инвариант №13 об id).

## Быстрый старт

1. Полностью прочитать `AGENTS.md`.
2. Проверить `git status -sb` и `git log -5 --oneline`.
3. Текущая ветка: `main`, remote: `origin` (`bqmaks/bqreport`).
4. Перед изменением публичного API сначала добавлять тест.
5. Перед публикацией запускать `devtools::test()` и
   `R CMD check --no-manual`.

На момент handoff рабочее дерево было чистым до обновления этого файла.
Последние опубликованные коммиты:

```text
88ee3c4 Document package installation
8af8e98 Add explicit custom method fallback chains
9b3ea21 Add vignettes and pkgdown site
fb968ae Add categorical plot colors
1e402b4 Add reporting tables and plots
```

## Архитектура

Пакет — tidyverse-native аналитический компилятор:

```text
bq_data + metadata + design
  -> selectors + analysis_rules
  -> validated analysis_plan
  -> statistical engines
  -> analysis_result
  -> tidy data / tables / plots
```

Источники истины — `bq_data` и `analysis_plan`. Вычисления, диагностика и
представление разделены. Все численные результаты хранятся без округления.

## Реализованные подсистемы

- `bq_data`, stable `var_id`, labels, roles (включая несколько ролей), types,
  references/events, distribution metadata и tidyselect selectors.
- Реконструкция metadata при основных dplyr-операциях.
- Описательные статистики для continuous/binary/nominal/ordinal/count:
  групповые и overall (`All patients`), шаблоны glue, `n`/`N`/`p`, MAD,
  skewness, kurtosis, normality и пользовательские `descriptive_function()`.
- Сравнения групп, omnibus tests/effect sizes, `against_reference()`,
  `all_pairwise()`, `consecutive_comparisons()` и
  `against_global_mean(exponentiate = ...)`.
- Linear/logistic/count/ordinal/multinomial models; explicit exponentiation,
  SE/CI, transformations and nonlinear covariate terms.
- Survival: KM, log-rank, Cox, RMST, survival quantiles, survival and
  cumulative-risk estimates; competing risks.
- Longitudinal long/wide design, LMM/GLMM/GEE, group × time and contrasts.
- Correlations: Pearson/Spearman/Kendall and extended methods, partial,
  weighted/repeated/resampled workflows, strata interactions and
  contrasts-of-contrasts.
- Reporting tables/plots, categorical colors as vector/named vector/function,
  `ru`/`en` groundwork, vignettes and pkgdown configuration.
- README contains GitHub installation via `pak`, `remotes`, and local
  `devtools::install()`.

## Пользовательские функции

- `analysis_function()` and `analysis_method()` validate stable output schemas.
- `method_selector()` is pre-fit only: candidates, reason and diagnostics are
  fixed during `validate_plan()` and never re-selected in `run_analysis()`.
- `comparison_function()`, `descriptive_function()`, correlation method and
  comparator contracts are available.
- Function id/hash and required packages are retained in plan/provenance.
- Malformed output is a typed error and is never repaired heuristically.

### Explicit runtime method chains

Commit `8af8e98` added:

```r
analysis_method_chain(id, methods, advance_on)

analysis_function(
  id, run, effect_measure, scale,
  fallback = list(...),
  advance_on = c("bq_error_convergence")
)
```

Properties:

- only declared condition classes allow advancement;
- all members must have identical effect measure, scale, model scale and
  exponentiation policy;
- `bq_error_invalid_engine_output` and contract errors never advance;
- `attempts(result)` contains every attempted method;
- plan stores `method_chain`, `fallback_conditions`, `executed_method`;
- provenance stores declared chain, actual method and `fallback_used`;
- failed attempts also appear in `issues()`.

Current limitation: runtime chains accept custom `analysis_function()` /
`analysis_method()` members only. Built-in engines are not chain members yet.

## Документация

Pkgdown site: <https://bqmaks.github.io/bqreport/>

Main vignettes:

- `vignettes/get-started.Rmd`
- `vignettes/descriptive-analysis.Rmd`
- `vignettes/models-and-contrasts.Rmd`
- `vignettes/correlation-analysis.Rmd`
- `vignettes/longitudinal-survival.Rmd`
- `vignettes/custom-functions.Rmd`
- `vignettes/examples-gallery.Rmd`

`custom-functions.Rmd` now contains an executable three-group custom HC3
linear model showing `analysis_method()`, estimates, omnibus test, custom
pairwise contrasts, Holm adjustment and standard reporting.

## Последняя верификация

После правок сессии 2026-08-13 (вечер):

```text
devtools::test(): 985 PASS, 0 FAIL, 0 SKIP
devtools::run_examples(): OK
R CMD check --no-manual: Status: OK
```

## Важный незакрытый пробел

В workflow сравнения трёх групп все четыре outcome-типа уже моделируются:

```text
continuous  -> linear_model
binary      -> logistic_model
nominal     -> multinomial_logistic_model
ordinal     -> ordinal_logistic_model
```

Но общий `contrasts()` сейчас полностью нормализует model-based group
contrasts прежде всего для continuous/binary paths. Для ordinal и multinomial
коэффициенты, SE, CI и omnibus likelihood-ratio tests доступны в
`estimates()`/`tests()`, однако target specifications (`all_pairwise`,
`against_reference`, `consecutive`, global mean where meaningful) ещё нужно
адаптировать к стабильной схеме `contrasts()` и multiplicity adjustment.

## Рекомендуемый следующий шаг

Сделать вертикальный срез model-based contrasts для ordinal и multinomial
outcomes:

1. До изменения API добавить контрактные тесты для трёх групп.
2. Явно определить estimand для каждой outcome category у multinomial model.
3. Нормализовать reference/all-pairwise/consecutive contrasts, SE и CI.
4. Применять explicit exponentiation и сохранять model/output scale.
5. Добавить Holm/другие `p.adjust` методы и non-estimable diagnostics.
6. Обновить three-group vignette полным исполняемым workflow, включая
   пользовательские функции на descriptive/selector/model/contrast этапах.
7. После тестов обновить pkgdown и опубликовать.

## GitHub/окружение

- SSH push в `origin/main` работает.
- `gh auth status` сообщал о недействительном CLI token; это не блокировало
  обычный `git push` по SSH.
- Не добавлять build/check artifacts: `/*.tar.gz`, `/*.Rcheck/` и `docs/`
  исключены через `.gitignore`.
