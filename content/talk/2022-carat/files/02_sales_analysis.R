library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)
library(here)

here()

# Импорт данных

?read_excel()

bikes_tbl <- read_excel(here(path = "00_data/xlsx/bikes.xlsx"))

bikeshops_tbl <- read_excel(here("00_data/xlsx/bikeshops.xlsx"))

orderlines_tbl <- read_excel(here("00_data/xlsx/orderlines.xlsx"))


# Просмотр данных

bikes_tbl

glimpse(bikes_tbl)

bikeshops_tbl

orderlines_tbl # связующие звено между bikes и bikeshops


# Объединение данных

orderlines_tbl
bikes_tbl

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>% 
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% 
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl

bike_orderlines_joined_tbl %>% glimpse()

# Обработка данных

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% 
    
    # Разделим description на category.1, category.2 и frame.material
    separate(description,
             into   = c("category.1", "category.2", "frame.material"),
             sep    = " - ",
             remove = TRUE) %>% 
    
    # Разделим location на city и state
    separate(location,
             into   = c("city", "state"),
             sep    = ", ",
             remove = FALSE) %>% 
    
    # Финальная цена
    mutate(total.price = price * quantity) %>% 
    
    # Реорганизация
    select(-...1, -location) %>% 
    select(-ends_with(".id")) %>% 
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
    
    # Смена позиции
    select(contains("date"), contains("id"), contains("order"),
           quantity, price, total.price,
           everything()) %>% 
    
    # Смена названий
    rename(order_date = order.date) %>% 
    set_names(names(.) %>% str_replace_all("\\.", "_")) #* регулярные выражения - надо учить!
                    # Что тут делает точка?

bike_orderlines_wrangled_tbl %>% glimpse()
    


# Аналитика

# Продажи по годам

# Шаг 1 - манипуляции

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
    
    # Добавим столбец года
    select(order_date, total_price) %>% 
    mutate(year = year(order_date)) %>% 
    
    # Группировка и агрегация по году
    group_by(year) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup() %>% # лучше всего использовать после группировки
    
    # $ Формат текста
    mutate(sales_text = scales::dollar(sales))

# Визуализация

palette_green()

sales_by_year_tbl %>% 
    
    # Настройка осей: год (х-абсцис) и продажи (y-ординат)
    ggplot(aes(x = year, y = sales)) +
    
    # Геометрия
    geom_col(fill = "#2c3e50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    
    # Форматирование
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Доход по годам",
        subtitle = "Возрастающий тренд",
        x = "",
        y = "Доход"
    )

# Продажи по годам и категории 2

# Манипуляции

sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>% 
    
    # Выбираем столбец и добавляем столбец года
    select(order_date, total_price, category_2) %>%
    mutate(year = year(order_date)) %>% 
    
    # Группировка и агрегация по году и category_2
    group_by(year, category_2) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    # Форматирование
    mutate(sales_text = scales::dollar(sales))

sales_by_year_cat_2_tbl

# Визуализация

sales_by_year_cat_2_tbl %>% 
    
    # Настройка осей x, y, и заливки fill
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    
    # Геометрия
    geom_col() +
    geom_smooth(method = "lm", se = FALSE) +
    
    # Facet: разибитие графика на подграфики по катерогии
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
    
    # Форматирование
    theme_tq() +
    scale_fill_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Доход по годам в Категории 2",
        subtitle = "Каждая категория продуктов имеет тенденцию к росту",
        x = "",
        y = "Доход",
        fill = "Вторичная категория продукта"
    )

# Запись результатов

fs::dir_create("00_data/data_wrangled_student")

# Excel

bike_orderlines_wrangled_tbl %>% 
    write_xlsx("00_data/data_wrangled_student/bike_orderlines.xlsx")

# CSV

bike_orderlines_wrangled_tbl %>% 
    write_csv("00_data/data_wrangled_student/bike_orderlines.csv")

# RDS

#* Используйте RDS, если хотите сохранить ЛЮБОЙ ОБЪЕКТ. Не только табличные данные.
# Сохраняйте модели, графики, что угодно! Это быстро и сохраняет точную структуру объекта
# (в отличие от Excel и CSV)

bike_orderlines_wrangled_tbl %>% 
    write_rds("00_data/data_wrangled_student/bike_orderlines.rds")

