# Контекст для следующей сессии

Обновлено: 2026-08-13 (Europe/Moscow). Рабочее дерево должно быть чистым
после коммита этой сессии. Ветка `main` локально опережает `origin/main`;
push в этой сессии не запрашивался.

## Быстрый старт

1. Полностью прочитать `AGENTS.md`.
2. Проверить `git status -sb` и `git log -5 --oneline`.
3. Текущая ветка: `main`, remote: `origin` (`bqmaks/bqreport`).
4. Перед изменением публичного API сначала добавлять тест.
5. Перед публикацией запускать `devtools::test()` и
   `R CMD check --no-manual`.

Последние локальные коммиты перед handoff-коммитом:

```text
61c6acd Combine compatible plans and add survival builders
e1f79d3 Add cumulative analysis plans and boot intervals
fc07399 Update session handoff
```

## Состояние верификации

```text
devtools::test(): полный набор проходит; lawstat-тест пропускается, если
{lawstat} не установлен.
Последний R CMD check --no-manual перед текущим срезом: Status OK при
_R_CHECK_FORCE_SUGGESTS_=false. Обычный check ранее блокировался только
отсутствующим optional {lawstat} при недоступной сети.
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

## Ключевые решения последней сессии

- Статистическое планирование стало композиционным: method, estimand,
  hypothesis, contrast orientation, multiplicity, omnibus, effect size и
  postprocessing — независимые спецификации без неявных defaults методов.
- `two_group_comparison()` исполняет Student/Welch и Brunner-Munzel.
  Brunner-Munzel имеет явные adapters `brunnermunzel_backend(permutation=)`
  и `lawstat_backend()`, estimands probability of superiority и `2p-1`,
  а также two-sided/superiority/NI/equivalence для допустимых backends.
- Omnibus и post hoc независимы: omnibus-only, pairwise-only либо оба.
  `model_postprocessing()` аккумулятивен; пользовательские шаги проходят
  через `postprocessing_function()`.
- `group_analysis_artifact` устраняет повторные вычисления. ANOVA test,
  eta-squared и Tukey используют один aov; Kruskal-Wallis и epsilon-squared
  используют один test/rank artifact; Fisher и Cramer's V используют одну
  contingency table, хотя статистические процедуры разные.
- Cox разделяет `baseline = common_baseline()/stratified_baseline()` и
  `subgroup = no_subgroup_analysis()/joint_interaction()/
  separate_subgroup_models()`. Joint fit возвращает interaction test и
  conditional HR; separate fit возвращает контейнер `cox_subgroup_fits` и
  не создаёт фиктивный interaction p-value.
- Добавлены optional engines: robust/quantile/beta regression,
  zero-inflated/hurdle counts, Fine-Gray, penalized Cox, NB GLMM.
- Все новые решения включаются в plan/provenance и digest analysis_id.

Старые решения по корреляциям остаются действующими:

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

## Важные незакрытые пробелы

1. Общий postprocessing pipeline пока полностью исполняется только в
   групповых сравнениях. Нужно подключить те же specs к regression/survival/
   longitudinal fits через единый read-only context и analysis artifacts.
2. Cox subgroup API реализован вертикальным срезом для одного modifier.
   Нужны preflight edge cases: пустая подгруппа, 0 событий, отсутствующий
   reference, singular/infinite coefficient, разные complete-case masks;
   также tidy estimates/tests по каждому separate fit, а не только contrasts.
3. `cox_model()` сохраняет совместимость с defaults. Пользователь просил
   в итоге убрать default statistical methods из planners; это ещё не
   проведено системно по `plan_analysis`, correlations, longitudinal и
   survival, чтобы не ломать весь API одним непроверенным изменением.

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

Сначала стабилизировать композиционный inference/postprocessing contract:

1. Ввести общий `analysis_artifact` contract (`fit`, derived caches,
   provenance, consumer compatibility) вместо локального только для groups.
2. Добавить независимые postprocessing consumers: effect sizes, marginal
   means, predictions, contrasts, diagnostics и custom functions.
3. Для Cox довести subgroup vertical slice: normalized estimates/tests,
   per-subgroup counts/events/issues, conditional-vs-marginal labels и
   contract tests с прямыми `coxph()` вызовами.
4. Затем распространить baseline/subgroup strategies на Fine-Gray и
   longitudinal models только там, где estimand математически определён.
5. После стабилизации удалить неявный выбор методов из planners и обновить
   все examples/vignettes миграционным разделом.

После этого вернуться к ранее запланированному model-based contrasts slice:

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
