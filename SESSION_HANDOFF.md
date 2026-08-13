# Контекст для следующей сессии

Обновлено: 2026-08-12.

## Как начать

1.  Полностью прочитать `AGENTS.md`: это источник архитектурных
    требований.
2.  Проверить `git status --short` и последние коммиты.
3.  Не менять уже принятые публичные контракты без предварительного
    теста.
4.  Перед новым коммитом запускать полный набор тестов и
    `R CMD check --no-manual`.

Текущая ветка: `main`.

Последний функциональный коммит:

``` text
6e7cb69 Add separation-aware logistic policy
```

## Цель пакета

`bqreport` — tidyverse-native R-пакет для воспроизводимого анализа
биомедицинских данных. Архитектура строится как аналитический
компилятор:

``` text
bq_data + metadata + design
  -> selectors + analysis_rules
  -> validated analysis_plan
  -> statistical engines
  -> analysis_result
  -> tidy data / tables / plots
```

Источником истины остаются `bq_data` и `analysis_plan`. Вычисление,
диагностика и представление должны оставаться разделёнными. Runtime
fallback запрещён.

## Уже реализовано

### Данные и метаданные

- `bq_data` как подкласс tibble.
- Непрозрачные стабильные `var_id` на UUID.
- Реестры переменных, ролей, outcomes, дизайна и contrasts.
- Несколько ролей у одной переменной.
- Labels, units, rounding rules и расширяемые поля словаря.
- Аналитические типы, references, event values и distribution metadata.
- Tidyselect-селекторы ролей, типов, распределений и статусов.
- Сохранение и инвалидирование метаданных при основных операциях dplyr.
- Обновление ссылок по стабильным ID при rename.

### Планирование и исполнение

- [`plan_analysis()`](https://bqmaks.github.io/bqreport/reference/plan_analysis.md)
  для outcome–predictor задач.
- Ковариаты, regression weights/IPW и model-based/robust variance.
- Cluster-robust variance для кластеров после matching.
- Независимый strata pipeline: отдельная задача для каждой наблюдаемой
  страты.
- Effect modifiers и формулы `predictor * modifier`.
- Preflight variation, missingness, references/events, sparse
  interaction cells, weights, clusters и required packages.
- [`approve_plan()`](https://bqmaks.github.io/bqreport/reference/approve_plan.md)
  для задач со статусом `review`.
- [`run_analysis()`](https://bqmaks.github.io/bqreport/reference/run_analysis.md)
  с `error = "collect" | "stop" | "warn"`.
- `analysis_result` и tidy accessors для estimates, contrasts, tests,
  diagnostics, issues и models.

### Методы и расширяемость

- Встроенные
  [`linear_model()`](https://bqmaks.github.io/bqreport/reference/linear_model.md)
  и
  [`logistic_model()`](https://bqmaks.github.io/bqreport/reference/linear_model.md).
- [`analysis_function()`](https://bqmaks.github.io/bqreport/reference/analysis_function.md)
  и
  [`analysis_method()`](https://bqmaks.github.io/bqreport/reference/analysis_method.md)
  для пользовательских engines.
- Строгая нормализация пользовательского
  [`analysis_output()`](https://bqmaks.github.io/bqreport/reference/analysis_output.md).
- Хеширование пользовательских функций и provenance.
- Явное `exponentiate` и разделение model/output scale.
- [`method_selector()`](https://bqmaks.github.io/bqreport/reference/method_selector.md)
  и
  [`method_choice()`](https://bqmaks.github.io/bqreport/reference/method_choice.md):
  - selector выполняется только в
    [`validate_plan()`](https://bqmaks.github.io/bqreport/reference/validate_plan.md);
  - получает подготовленный `analysis_context`;
  - выбирает только из именованных candidates;
  - сохраняет reason и diagnostics;
  - [`run_analysis()`](https://bqmaks.github.io/bqreport/reference/run_analysis.md)
    selector повторно не вызывает.
- [`firth_logistic()`](https://bqmaks.github.io/bqreport/reference/firth_logistic.md)
  на optional backend `logistf`.
- [`separation_logistic()`](https://bqmaks.github.io/bqreport/reference/separation_logistic.md)
  использует `detectseparation` на preflight и фиксирует `glm` либо
  Firth без runtime fallback.

### Трансформации и contrasts

- Структурированные scalar transformations:
  [`per()`](https://bqmaks.github.io/bqreport/reference/per.md),
  [`log2_transform()`](https://bqmaks.github.io/bqreport/reference/per.md),
  [`log10_transform()`](https://bqmaks.github.io/bqreport/reference/per.md)
  и
  [`transformation_function()`](https://bqmaks.github.io/bqreport/reference/transformation_function.md).
- Трансформации применяются к predictor/covariates и сохраняются в плане
  и provenance.
- Независимое model coding через
  [`set_coding()`](https://bqmaks.github.io/bqreport/reference/set_coding.md).
- [`against_reference()`](https://bqmaks.github.io/bqreport/reference/against_reference.md)
  и
  [`within_levels()`](https://bqmaks.github.io/bqreport/reference/within_levels.md).
- Пользовательские попарные сравнения через
  [`comparison_function()`](https://bqmaks.github.io/bqreport/reference/comparison_function.md).
- Условные эффекты внутри уровней modifier.
- Omnibus-тесты взаимодействия.
- Model-based, robust и cluster-robust covariance используются и для
  условных эффектов.

## Последняя проверка

Перед handoff успешно выполнены:

``` text
devtools::test(): 370 PASS, 0 FAIL, 0 WARN, 0 SKIP
R CMD check --no-manual: Status: OK
```

Firth-оценки, профильные confidence intervals и p-values тестами сверены
с прямым
[`logistf::logistf()`](https://rdrr.io/pkg/logistf/man/logistf.html).

Optional packages `detectseparation` и `logistf` находятся в `Suggests`.
`logistf` проверяется только после выбора Firth candidate; обычную
GLM-ветвь не должно блокировать отсутствие неиспользуемого backend.

## Пример текущего pipeline

``` r

data <- raw_data |>
  as_bq_data(metadata = dictionary) |>
  set_outcome(response, type = "binary", event = 1) |>
  set_predictor(treatment, type = "nominal", reference = "Placebo") |>
  set_transformation(age, per(10)) |>
  set_weight(iptw, type = "ipw") |>
  set_cluster(matched_set, type = "matched_set") |>
  set_comparisons(
    treatment,
    comparisons = against_reference("Placebo"),
    adjust = "holm"
  )

rules <- analysis_rules(
  where_binary() ~ separation_logistic(exponentiate = TRUE)
)

plan <- data |>
  plan_analysis(
    outcomes = all_outcomes(),
    predictors = all_predictors(),
    covariates = age,
    weights = iptw,
    cluster = matched_set,
    strata = sex,
    rules = rules
  ) |>
  validate_plan(data)

result <- run_analysis(plan, data, error = "collect")
result |> estimates()
result |> contrasts()
result |> diagnostics()
```

## Важные архитектурные ограничения

- Не передавать пользовательским функциям полный изменяемый `bq_data`.
- Не пытаться эвристически исправлять malformed output пользовательской
  функции.
- Не округлять числа в вычислительном слое.
- Не хранить локализованный текст в численных результатах.
- Не смешивать coding коэффициентов и целевые comparisons.
- Не считать robust/cluster-robust covariance только свойством таблицы:
  она должна использоваться во всех производных оценках.
- Не запускать selector в
  [`run_analysis()`](https://bqmaks.github.io/bqreport/reference/run_analysis.md)
  и не переключать engine после ошибки fit.
- В реестрах и планах ссылаться на стабильные IDs, имена обновлять при
  validation.

## Рекомендуемый следующий вертикальный срез

Начать вычислительный слой описательной статистики и сравнений групп из
v0.2:

1.  Сначала определить tidy-схемы `descriptives` и group comparisons.
2.  Добавить отдельный тип задач в `analysis_plan`, не встраивая расчёт
    в regression engine.
3.  Реализовать continuous descriptives (`n`, missing, mean/SD,
    median/IQR, min/max) без округления.
4.  Реализовать categorical counts/proportions с явными знаменателями.
5.  Поддержать общий результат и результаты по группам/стратам.
6.  Затем добавить базовые group tests как отдельные estimates/tests, с
    effect estimates первичнее p-values.
7.  Только после стабильного вычислительного контракта переходить к
    первому `gt`-представлению.

Перед реализацией нужно согласовать с пользователем минимум два решения:

- должна ли одна descriptives-задача возвращать несколько статистик
  длинным форматом (`statistic`, `value`) или использовать стабильные
  широкие поля;
- какие знаменатели для процентов считать обязательными: `n_analyzed`,
  внутри группы и/или общий eligible denominator.

## Более поздние незакрытые области

- Survival outcomes, Kaplan–Meier, log-rank, Cox и PH diagnostics.
- Longitudinal long/wide design, canonical long analysis frame,
  LMM/GLMM/GEE.
- Полноценные outcome/design registries за пределами текущего первого
  среза.
- Публикационные таблицы и графики.
- Локали `ru`/`en` для presentation layer.
- Join/pivot edge cases и специальные методы для неоднозначных reshapes.
