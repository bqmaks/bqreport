# Контекст для следующей сессии

Обновлено: 2026-08-13, вечер (Europe/Moscow). Рабочее дерево чистое,
`main` синхронизирован с `origin/main`, CI зелёный.

## Быстрый старт

1. Полностью прочитать `AGENTS.md`.
2. Проверить `git status -sb` и `git log -5 --oneline`.
3. Текущая ветка: `main`, remote: `origin` (`bqmaks/bqreport`).
4. Перед изменением публичного API сначала добавлять тест.
5. Перед публикацией запускать `devtools::test()` и
   `R CMD check --no-manual`.

Последние опубликованные коммиты:

```text
339ff4c Fix correlation resampling and make identifiers deterministic
d7061c3 Update session handoff
88ee3c4 Document package installation
8af8e98 Add explicit custom method fallback chains
9b3ea21 Add vignettes and pkgdown site
```

## Состояние верификации (commit 339ff4c)

```text
devtools::test(): 985 PASS, 0 FAIL, 0 SKIP
devtools::run_examples(): OK
R CMD check --no-manual: Status: OK (локально)
GitHub Actions: R-CMD-check (release + oldrel-1) success, pkgdown success
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

Все идентификаторы детерминированы (`R/ids.R`, `bq_id()` — digest
содержимого): одинаковый вход компилируется в идентичные планы и
результаты между запусками. `var_id` = `var_<имя столбца>` с
детерминированной суффиксацией, если освобождённое имя переиспользуется
(`uniquify_fresh_ids()` в `dplyr_reconstruct`). Специализированные
планировщики неймспейсят `analysis_id` через `refine_analysis_id()`.
Случайные id (uuid) запрещены — инвариант №13 в AGENTS.md.

### Карта файлов (после разрезания 339ff4c)

- `R/analysis-plan.R` — plan_analysis, analysis_plan_row, validate_plan,
  approve_plan.
- `R/analysis-result.R` — только run_analysis (диспетчер по analysis_type).
- `R/engines-builtin.R` — analysis frame, fit builtin engines, tidy
  extraction, диагностика.
- `R/engines-custom.R` — custom engines, method chains, контрактная
  валидация вывода.
- `R/result-components.R` — прототипы компонентов, provenance_row,
  issue_row, engine conditions.
- `R/result-accessors.R` — estimates()/contrasts()/tests()/issues() и пр.
- `R/correlation-methods.R` — конструкторы методов, correlation_output,
  comparator, resampling (`resample_correlation_context()` — cluster
  bootstrap; `permute_correlation_context()` — within-subject permutation).
- `R/correlation-plan.R` — plan_correlations, preflight.
- `R/correlation-execute.R` — execute_correlation, встроенные оценщики.
- `R/correlation-interactions.R` — interaction tests, Fisher z comparator.
- `R/ids.R` — bq_id, refine_analysis_id, uniquify_fresh_ids.

## Ключевые решения прошлой сессии (2026-08-13, вечер)

- `resampled_correlation()`: bootstrap ресемплирует веса вместе с
  наблюдениями; для методов с subject id — cluster bootstrap целыми
  субъектами, перестановки только внутри субъекта.
- Spearman CI: SE Bonett–Wright `sqrt((1 + r^2/2)/(n - k - 3))`,
  `ci_method = "fisher_z_bonett_wright"`.
- Методы без inference (`biweight_correlation()`,
  `provides_inference = FALSE`) валидируются в статус `review`; запуск
  после `approve_plan()` или через `resampled_correlation()`. При
  re-validation внутри `run_analysis()` учитывается `plan$approved`.
- Минимум наблюдений resampled-метода берётся от base method id.
- Kendall CI — грубая нормальная аппроксимация; задокументировано,
  рекомендован resampling.
- `@examples` у ключевых экспортов; NEWS.md ведётся; автор в DESCRIPTION
  реальный; добавлен workflow `.github/workflows/R-CMD-check.yaml`.

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

## Пользовательские функции

- `analysis_function()` and `analysis_method()` validate stable output schemas.
- `method_selector()` is pre-fit only: candidates, reason and diagnostics are
  fixed during `validate_plan()` and never re-selected in `run_analysis()`.
- `comparison_function()`, `descriptive_function()`, correlation method and
  comparator contracts are available.
- Function id/hash and required packages are retained in plan/provenance.
- Malformed output is a typed error and is never repaired heuristically.

### Explicit runtime method chains

`analysis_method_chain(id, methods, advance_on)` и `fallback`/`advance_on`
у `analysis_function()`: продвижение только по объявленным condition
classes; члены цепочки обязаны иметь идентичные effect measure/scale/
exponentiation; `attempts(result)` хранит каждую попытку; provenance —
declared chain, actual method, `fallback_used`.

Ограничение: членами runtime chain могут быть только пользовательские
`analysis_function()`/`analysis_method()`; встроенные engines — ещё нет.

## Документация

Pkgdown site: <https://bqmaks.github.io/bqreport/>

Виньетки: `get-started`, `descriptive-analysis`, `models-and-contrasts`,
`correlation-analysis`, `longitudinal-survival`, `custom-functions`
(включает исполняемый three-group custom HC3 workflow), `examples-gallery`.

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

Мелкие кандидаты в бэклог:

- coverage workflow (covr локально не установлен; воркфлоу не добавлен,
  чтобы не публиковать непроверенный YAML);
- `set_comparisons()` дважды для одного predictor сейчас дописывает
  дубликат в contrast registry — рассмотреть replace-семантику;
- встроенные engines как члены `analysis_method_chain`.

## GitHub/окружение

- SSH push в `origin/main` работает; `gh` CLI тоже работает
  (`gh run list` показывает статусы Actions).
- CI: `.github/workflows/R-CMD-check.yaml` (ubuntu, release + oldrel-1)
  и `pkgdown.yaml` (деплой на gh-pages).
- Не добавлять build/check artifacts: `/*.tar.gz`, `/*.Rcheck/` и `docs/`
  исключены через `.gitignore`.
- При правках публичного API: NEWS.md обновлять в том же коммите.
