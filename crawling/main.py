from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from webdriver_manager.chrome import ChromeDriverManager

import csv
import time


# 크롬 드라이버 실행
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()))
wait = WebDriverWait(driver, 5)

# 로그인
driver.get('https://www.jobplanet.co.kr/users/sign_in')

time.sleep(1)

driver.find_element(By.XPATH, '//*[@id="user_email"]').send_keys('** 이메일 **')
driver.find_element(By.XPATH, '//*[@id="user_password"]').send_keys('** 비밀번호 **')
driver.find_element(By.XPATH, '//*[@id="signInSignInCon"]/div[2]/div/section[3]/fieldset/button').click()

time.sleep(1)

# 기업 리뷰
url = 'https://www.jobplanet.co.kr/companies/{company_id}/reviews?occupation_1%5B%5D=11600&year%5B%5D=2024&year%5B%5D=2023&year%5B%5D=2022&year%5B%5D=2021&year%5B%5D=2020&page={page}'

company = {
    '네이버': 42217,
    '카카오': 93880,
    '라인': 89255,
    '쿠팡': 87444,
    '배달의민족': 61420,
}

for company_name, company_id in company.items():
    data = []

    for page in range(1, 101):
        try:
            driver.get(url.format(company_id=company_id, page=page))

            time.sleep(1)

            if driver.find_elements(By.CLASS_NAME, 'no_result'):
                break

            for review in driver.find_elements(By.CLASS_NAME, 'video_ad_content'):
                title = review.find_element(By.CLASS_NAME, 'us_label').text
                [pros, cons, wish] = review.find_element(By.CLASS_NAME, 'tc_list').find_elements(By.CLASS_NAME, 'df1')
                pros = pros.text
                cons = cons.text
                data.append([title, pros, cons])
        except Exception as e:
            print(e.args[0])

    with open('잡플래닛_리뷰_%s.csv' % company_name, mode='w', encoding='utf-8', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['title', 'pros', 'cons'])
        for row in data:
            try:
                writer.writerow(row)
            except Exception as e:
                print(e.args[0], data)
