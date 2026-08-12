# AGENTS.md

## Проект

Разрабатывается R-пакет для воспроизводимого анализа биомедицинских данных:

- препроцессинг и метаданные на основе `labelled`;
- описательная статистика и сравнение групп;
- однофакторные регрессионные модели;
- лонгитудинальный анализ;
- анализ выживаемости;
- tidy-результаты, публикационные таблицы и графики.

Пакет должен быть максимально tidyverse-native. Он не является форком или обёрткой над `gtsummary`: вычислительное ядро, классы и публичный контракт собственные. Удачные UX-паттерны `gtsummary` можно использовать как ориентир.

## Архитектурная модель

Проект строится как аналитический компилятор:

```text
bq_data + metadata + design
  -> selectors + analysis_rules
  -> validated analysis_plan
  -> statistical engines
  -> analysis_result
  -> tidy data / tables / plots
```

Источником истины является связка `bq_data + analysis_plan`, а не оформленная таблица и не объект конкретной модели.

Настройка **column-driven, но не column-only**:

- свойства переменной задаются от столбца;
- связи между столбцами хранятся в спецификации дизайна;
- survival outcome — составной объект `time + event`;
- лонгитудинальные данные могут поступать в long- или wide-формате;
- в long-формате дизайн задаёт как минимум `id + time`, а в wide-формате повторный outcome связывает набор столбцов с явными значениями времени;
- contrasts/comparisons — отдельные estimand-спецификации предиктора.

## Инварианты

1. Входные данные остаются tibble и работают в pipe-конвейерах.
2. Выбор переменных использует tidyselect.
3. Публичные вычислительные результаты представлены tidy tibbles.
4. Числа хранятся без округления; форматирование выполняется только при выводе.
5. Метод, основание выбора, estimand, scale и warnings фиксируются в плане и результате.
6. До выполнения batch-анализа пользователь может просмотреть и изменить план.
7. Никаких молчаливых замен метода при ошибке или несходимости.
8. Метаданные сохраняются только когда остаются корректными; устаревшие свойства инвалидируются.
9. Оценки эффекта и доверительные интервалы первичны; p-values дополнительны.
10. Вычисление, диагностика и представление разделены.
11. Лонгитудинальные engines работают с каноническим long analysis frame; преобразование из wide задаётся явно, проверяется до моделирования и фиксируется в плане и provenance без изменения исходного `bq_data`.
12. Пользователь может передавать функции выбора метода и статистические engines через валидируемые публичные контракты; выбор и идентичность функции фиксируются в плане и provenance.

## Доменные объекты

### `bq_data`

Подкласс tibble с реестрами:

- `variable_registry` — свойства столбцов;
- `outcome_registry` — составные outcomes;
- `design_registry` — дизайн исследования;
- `contrast_registry` — comparisons/estimands.

Доступ только через `variables()`, `outcomes()`, `designs()` и `contrasts()`. Публичный API не должен требовать прямой работы с атрибутами.

### `analysis_rules`

Декларативное сопоставление:

```text
outcome type + predictor type + design -> method_spec
```

Приоритет: конкретная пара outcome–predictor → конкретный outcome → пользовательское selector-rule → профиль проекта → системный default. Два правила одинакового приоритета — ошибка неоднозначности.

Правило может возвращать как конкретный `method_spec`, так и `method_selector`. `method_selector` выполняет data-dependent выбор из заранее объявленных методов на этапе компиляции/preflight; после выбора план обязан содержать конкретный метод.

### `analysis_plan`

Подкласс tibble; одна строка — одно аналитическое задание. Минимальная схема:

```text
analysis_id, analysis_type, outcome_id, outcome, predictor,
design, data_layout, reshape_spec, method_policy, selector_id,
candidate_methods, method, engine, estimator, ci_method, formula,
family, link, effect_measure, selection_reason,
selection_diagnostics, function_id, function_hash, required_packages,
contrast_id, adjust_method, missing_policy, confidence_level,
status, reason
```

`reshape_spec` — структурированная спецификация преобразования, а не исполняемый пользовательский код. Для wide-лонгитудинальных задач она хранит стабильные `var_id` исходных столбцов, соответствующие значения времени, baseline, `time_scale` и правила переноса метаданных.

`candidate_methods`, `selection_diagnostics` и другие структурированные значения допустимо хранить list-columns. В validated plan поле `method` всегда конкретно: unresolved selector получает статус `invalid`, а не переносится в `run_analysis()`.

Статусы: `ready`, `review`, `warning`, `invalid`, `excluded`.

### `analysis_result`

Составной объект:

```text
plan, models, estimates, contrasts, tests, descriptives,
diagnostics, issues, provenance
```

Все компоненты, кроме моделей, — tibbles. Нужны accessors: `estimates()`, `contrasts()`, `tests()`, `diagnostics()`, `issues()`, `models()`.

## Метаданные

`labelled` используется для variable labels, value labels и специальных пропусков. Аналитические свойства хранятся в отдельном tidy-реестре.

`variable_registry` поддерживает:

```text
var_id, name, label, unit, role, type, storage_type,
distribution, reference, event_value, transformation,
missing_policy, source, locked, status
```

Разделять физический тип R, аналитический тип, диагностику распределения и утверждённый distribution profile.

Типы:

```text
continuous, binary, ordinal, nominal, count,
date, datetime, identifier, unknown
```

Роли:

```text
outcome, predictor, group, id, time, visit, event,
offset, weight, cluster, stratum, auxiliary
```

Настройка хранит источник: `explicit`, `dictionary`, `inferred`, `diagnostic`, `default`. Во внутренних ссылках использовать стабильный `var_id`, чтобы корректно поддерживать `rename()`.

## Ожидаемый API

```r
data <- raw_data |>
  as_bq_data() |>
  apply_dictionary(dictionary) |>
  set_role(patient_id, "id") |>
  set_role(visit, "time") |>
  set_predictor(treatment, type = "nominal", reference = "Placebo") |>
  set_predictor(age, type = "continuous", effect = per(10)) |>
  set_outcome(c(bmi, crp, quality_score), type = "continuous") |>
  set_distribution(crp, "skewed") |>
  set_outcome(response, type = "binary", event = 1)
```

Селекторы:

```r
all_outcomes(); all_predictors(); all_groups()
where_role(); where_type(); where_distribution()
where_binary(); where_continuous(); where_ordinal()
where_nominal(); where_count(); where_survival()
where_status(); where_inferred()
```

Они должны компоноваться средствами tidyselect:

```r
where_continuous() & where_gaussian()
all_outcomes() & !matches("_exploratory$")
```

## Дизайн и контрасты

Long-формат настраивается непосредственно через столбцы `id` и `time`:

```r
data <- data |>
  set_longitudinal_design(
    id = patient_id,
    time = visit,
    group = treatment,
    layout = "long",
    baseline = "Visit 0",
    time_scale = "categorical"
  )
```

Wide-формат является полноценным допустимым входом. Соответствие столбцов визитам задаётся явно в составном longitudinal outcome:

```r
data <- data |>
  set_longitudinal_design(
    id = patient_id,
    group = treatment,
    layout = "wide"
  ) |>
  add_longitudinal_outcome(
    bmi,
    values = c(bmi_v0, bmi_v1, bmi_v2),
    time = c("Visit 0", "Visit 1", "Visit 2"),
    baseline = "Visit 0",
    time_scale = "categorical",
    type = "continuous"
  ) |>
  add_survival_outcome(
    overall_survival,
    time = os_time,
    event = death,
    event_value = 1,
    time_unit = "months"
  )
```

Не выводить соответствие `столбец -> время` молча из имён. Допустим отдельный helper для построения предлагаемой спецификации по шаблону, но результат должен получать статус `review` до явного подтверждения. Разные outcomes могут иметь разные наборы визитов. При преобразовании переносить label, unit, value labels и специальные пропуски только когда соответствующие метаданные совместимы; конфликты должны давать диагностируемый issue.

Исходный wide `bq_data` не преобразуется на месте. Планировщик создаёт для конкретного задания внутренний канонический long analysis frame с техническими столбцами outcome и time, а точное преобразование сохраняет в `reshape_spec` и `provenance`.

Не считать LMM/GLMM и GEE взаимозаменяемыми: выбор должен отражать estimand и быть явным.

Строго разделять model coding и целевые comparisons:

```r
data |>
  set_coding(treatment, coding = "treatment", reference = "Placebo") |>
  set_comparisons(
    treatment,
    comparisons = against_reference("Placebo"),
    adjust = "holm"
  )
```

Публичный `contrast_spec` не должен зависеть от `emmGrid`; `emmeans` используется через adapter.

## Распределения и автоматизация

Не выбирать параметрический/непараметрический анализ только по p-value теста нормальности. Разделять диагностику, рекомендацию пакета и утверждённую пользователем настройку.

```r
profile <- data |> profile_distributions(where_continuous())
data <- data |> apply_distribution_profile(profile, mode = "reviewed")
```

По умолчанию предлагать метод для просмотра, а не применять его молча.

## Планирование и исполнение

```r
rules <- analysis_rules(
  where_continuous() & where_gaussian() ~ linear_model(),
  where_continuous() & where_skewed() ~ robust_or_rank_model(),
  where_binary() ~ logistic_model(),
  where_survival() ~ cox_model()
)

plan <- data |>
  plan_analysis(
    outcomes = all_outcomes(),
    predictors = all_predictors(),
    rules = rules
  )

validate_plan(plan, data)
result <- run_analysis(plan, data, error = "collect")
```

Preflight проверяет переменные, variation, reference/event values, группы, число событий, estimability контрастов, объём complete cases и формулу. Для лонгитудинальных задач дополнительно проверяются полнота и однозначность wide mapping, совместимость типов и метаданных повторных столбцов, допустимость baseline/time и уникальность `id + time` уже в каноническом long analysis frame.

Если правило содержит `method_selector`, планировщик строит тот же model frame и model matrix, которые получит engine, выполняет selector, валидирует возвращённый `method_choice` и записывает выбранный метод, основание и pre-fit diagnostics в план. Пользователь должен иметь возможность проверить это решение до `run_analysis()`.

## Статистические движки

Единый внутренний контракт:

```r
fit_engine(spec, data)
tidy_estimates(fit, spec)
tidy_tests(fit, spec)
compute_contrasts(fit, spec)
check_diagnostics(fit, spec)
```

Движок не форматирует вывод.

| Задача | Backend |
|---|---|
| Descriptives | собственные функции на `dplyr` |
| Базовые тесты | `stats` |
| Linear/logistic models | `stats::lm()`, `stats::glm()` |
| Survival | `survival` |
| Mixed models | `lme4`, позднее `glmmTMB` |
| GEE | `geepack` |
| Контрасты | `emmeans` |
| Tidy extraction | `broom`, `broom.mixed`, собственные adapters |

Нормализовать вывод внешних пакетов во внутреннюю стабильную схему; не считать текущую схему `broom` внутренним контрактом.

## Пользовательские функции и выбор метода

Расширяемость поддерживается двумя отдельными контрактами.

### Data-dependent `method_selector`

Selector выбирает метод до fit из явно перечисленных candidates. Он получает read-only `analysis_context` и возвращает только `method_choice`:

```r
separation_policy <- method_selector(
  id = "separation_glm_or_firth",
  candidates = list(
    glm = logistic_model(engine = "glm"),
    firth = firth_logistic(engine = "logistf")
  ),
  select = function(context) {
    separation_fit <- stats::glm(
      formula = context$formula,
      data = context$model_frame,
      family = stats::binomial("logit"),
      method = detectseparation::detect_separation
    )

    separated <- any(is.infinite(stats::coef(separation_fit)))

    method_choice(
      method = if (separated) "firth" else "glm",
      reason = if (separated) {
        "Detected complete or quasi-complete separation"
      } else {
        "Maximum-likelihood estimates are finite"
      },
      diagnostics = tibble::tibble(separation = separated)
    )
  }
)

rules <- analysis_rules(
  where_binary() ~ separation_policy
)
```

Это явная pre-fit policy, а не fallback: `detectseparation` диагностирует данные, план фиксирует `glm` или `logistf`, затем запускается только выбранный engine. Ошибка или несходимость выбранного engine регистрируется как issue и не вызывает молчаливого переключения.

Selector обязан:

- выбирать только из объявленных `candidates`;
- возвращать стабильный `method_choice(method, reason, diagnostics)`;
- не изменять данные, глобальные options или внешнее состояние;
- объявлять необходимые пакеты и значимые настройки;
- оставлять диагностические числа неокруглёнными.

### Пользовательский статистический engine

Полный контракт регистрируется через конструктор, а не через неформальный набор функций:

```r
custom_method <- analysis_method(
  id = "my_logistic_method",
  fit = function(context) {
    my_fit(context$formula, data = context$model_frame)
  },
  tidy_estimates = function(fit, context) {
    # tibble стандартной схемы estimates
  },
  tidy_tests = function(fit, context) {
    # tibble стандартной схемы tests
  },
  compute_contrasts = function(fit, context) {
    # tibble стандартной схемы contrasts
  },
  diagnose = function(fit, context) {
    # tibble стандартной схемы diagnostics
  }
)
```

Для атомарного engine допустим сокращённый контракт:

```r
custom_method <- analysis_function(
  id = "my_method",
  run = function(context) {
    analysis_output(
      model = fit,
      estimates = estimates,
      tests = tests,
      contrasts = contrasts,
      diagnostics = diagnostics,
      issues = issues
    )
  }
)
```

`analysis_context` содержит только подготовленные входы конкретного задания:

```text
analysis_id, formula, model_frame, model_matrix, response,
weights, offset, outcome_spec, predictor_spec, design_spec,
contrast_specs, missing_counts, confidence_level
```

Не передавать пользовательской функции изменяемый полный `bq_data`. Возвращаемые tibbles валидируются по внутренним схемам, типам, `analysis_id`, effect measure и scale. Нарушение контракта — типизированная ошибка, а не попытка угадать или исправить результат.

`function_id`, хеш тела/значимых настроек, версии необходимых пакетов и выбранный method записываются в provenance. Хеш помогает обнаруживать изменение реализации, но не заменяет версионирование исходного кода; для воспроизводимых workflows предпочтительны именованные функции из пакета или проекта.

Основные поля `estimates`:

```text
analysis_id, outcome, predictor, term, level, estimate,
std_error, conf_low, conf_high, statistic, df, p_value,
effect_measure, scale, n, n_events, method
```

Основные поля `contrasts`:

```text
analysis_id, outcome, predictor, contrast, numerator,
denominator, estimate, conf_low, conf_high, p_value,
p_adjusted, adjust_method, effect_measure, scale
```

Всегда учитывать `n_total`, `n_eligible`, `n_analyzed`, `n_missing_outcome`, `n_missing_predictor`.

## Tidyverse-совместимость

В `Imports` использовать отдельные пакеты, не метапакет `tidyverse`:

```text
cli, dplyr, forcats, labelled, lifecycle, purrr, rlang,
stringr, tibble, tidyr, tidyselect, vctrs, broom
```

Специализированные backends по возможности держать в `Suggests`. Для разделения в логистической регрессии предусмотреть опциональные `detectseparation` и `logistf`; отсутствие required package выявлять до запуска соответствующего задания.

Для `bq_data` реализовать и тестировать `dplyr_reconstruct()`, `dplyr_row_slice()`, `dplyr_col_modify()`, `[.bq_data` и `names<-.bq_data`.

| Операция | Метаданные |
|---|---|
| `filter()`, `slice()`, `arrange()` | сохранить |
| `select()` | удалить записи исключённых переменных |
| `rename()` | обновить все ссылки |
| `mutate()` новой переменной | создать запись `review` |
| `mutate()` существующей | проверить тип, инвалидировать устаревшее |
| joins | объединить реестры, выявить конфликты |
| `summarise()`, `reframe()` | вернуть обычный tibble |
| pivots | специальные методы; не сохранять неоднозначное молча; аналитическое wide-to-long преобразование описывать через `reshape_spec` |

## Ошибки и диагностика

Использовать типизированные conditions и `cli`, например:

```text
bq_error_invalid_outcome
bq_error_ambiguous_rule
bq_error_missing_reference
bq_error_invalid_method_contract
bq_error_invalid_method_choice
bq_error_invalid_engine_output
bq_error_missing_backend
bq_warning_sparse_cells
bq_warning_convergence
bq_warning_non_estimable_contrast
```

Сообщение объясняет, что произошло, для какого анализа, почему и как исправить. В batch-режиме поддержать `error = "stop" | "collect" | "warn"`.

## Представление

Результаты работают с обычными глаголами `dplyr`. Табличный и графический слои отдельны:

```r
result |> contrasts() |> filter(p_adjusted < 0.05)
result |> tbl_comparison() |> as_gt()
result |> tbl_regression() |> as_flextable()
result |> plot_forest()
result |> plot_longitudinal()
result |> plot_survival()
```

Поддержать локали `ru` и `en`. Не хранить локализованный или округлённый текст в численных результатах.

## Тестирование

Использовать `testthat`. Каждый встроенный и пользовательский engine обязан проходить общий набор контрактных тестов стандартной схемы, `analysis_id`, CI, scale/effect measure, отсутствия округления и обработки warnings/errors.

Численные результаты сверять с прямыми вызовами backend. Snapshot-тесты использовать преимущественно для сообщений и оформленных таблиц.

Обязательные edge cases: один уровень binary outcome, predictor без variation, пустая группа, полностью пропущенный outcome, missing reference, perfect separation, отсутствие событий, дубли `id + visit`, singular mixed model, non-estimable contrast. Для wide-лонгитудинальных данных отдельно тестировать несовпадение длин `values` и `time`, повторяющиеся или неизвестные значения времени, отсутствующий baseline, несовместимые типы/метаданные столбцов и дубли `id + time`, возникающие после преобразования.

Для `method_selector` тестировать выбор каждой candidate-ветви, повторяемость решения, неизвестный method id, malformed diagnostics, ошибку selector и отсутствие required package. Для политики separation отдельно сверять выбор `glm` на overlapping data и `logistf` при complete/quasi-complete separation. `run_analysis()` не должен повторно вызывать selector или менять зафиксированный метод.

## Правила для coding agents

1. Перед изменением публичного API создать или обновить тест желаемого поведения.
2. Не добавлять метод без явного estimand, схемы результата и диагностик.
3. Не смешивать расчёт, округление и рендеринг.
4. Для NSE использовать только стандартные механизмы `rlang`/tidyselect.
5. Не использовать недокументированные internals зависимостей.
6. Во внутренних реестрах ссылаться на стабильные `var_id`, а не только на имена.
7. Не сохранять устаревшие метаданные после изменения столбца.
8. Не выполнять runtime fallback. Data-dependent выбор допустим только как объявленный pre-fit `method_selector` с candidates, reason и diagnostics, зафиксированными в плане.
9. Ошибки должны быть предметными и исправимыми.
10. Публичные функции документировать через roxygen2 и покрывать тестами.
11. Новые зависимости добавлять только при явной пользе; тяжёлые engines делать опциональными.
12. Предпочитать работающий вертикальный срез множеству API-заглушек.
13. Пользовательские функции принимать только через валидируемые constructors контрактов; не выводить семантику произвольного return value эвристически.

## Дорожная карта

- **v0.1:** каркас пакета, `bq_data`, реестры, `labelled`, типы/роли/селекторы, dplyr-реконструкция, минимальный plan и preflight.
- **v0.2:** descriptives, сравнение групп, univariable linear/logistic models, контракт пользовательских engines, pre-fit `method_selector`, опциональная маршрутизация `detectseparation -> glm/logistf`, базовые contrasts, `analysis_result`, первый `gt`-вывод.
- **v0.3:** survival outcomes, Kaplan–Meier, log-rank, Cox, PH diagnostics, survival/forest plots.
- **v0.4:** longitudinal design для long и wide inputs, проверяемый wide-to-long analysis frame, LMM/GLMM, GEE, `group × time`, change contrasts, longitudinal plots.
- **v0.5:** multinomial, ordinal, count models, survey weights, multiple imputation, дополнительные backends.

## Первый вертикальный срез

```text
as_bq_data()
  -> set_outcome()/set_predictor()
  -> selector-based plan_analysis()
  -> validate_plan()
  -> linear/logistic engine
  -> analysis_result + tidy accessors
  -> простая gt-таблица
```

Критерий готовности: каждое аналитическое решение можно просмотреть, изменить и воспроизвести; численные результаты совпадают с прямыми вызовами базовых R-моделей.
