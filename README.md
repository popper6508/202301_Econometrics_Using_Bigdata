# 2023 1학기 Bigdata Analysis Picture

## Project 절차

### 자료 가공 과정
 1. 선거 데이터 정리
    - 선거관리위원회 선거 데이터는 도-특별시 / 시군구 / 구 - 동읍면 / 선거 장소 순
    - 이중 시군구 단위를 기준으로 데이터를 정리하기 위해 동읍면의 ‘합계’를 기준으로 데이터 정리        
    - 검증과정을 통해 누락된 데이터를 채우는 과정을 거침.
    - 분석에 필요한 핵심 데이터 추출 : 야당 득표율 (2022년 기준 야당) 및 시군구별 투표율
 2. 통계청 및 교통 빅데이터 등 다양한 시군구 단위 지역 데이터 병합
    - 시군구 단위(구가 존재하는 시의 경우, ‘시 단위’, 특별시는 구 단위)로 데이터 병합 
    - 교통 데이터 전처리
    - Pivot 함수 활용하여 데이터 정리  
    - 비율로 바꾸는 것이 필요한 데이터 비율로 정리
    - 정리 후 병합
    - **병합 후 결측값 및 inf 확인**

- 기초 통계량과 상관분석
    1. 자료 분포
    2. 상관분석
    3. 기초통계량
- **적합도 검정**
    - ***KMO 측도*** : 
        - 각 변수 간 유의미한 상관관계가 있는지 확인
        
    - ***Bartlett 검정*** 
        - ‘구형성 검정’ : 자료가 일정한 관계 없이 퍼져 있는지 검정 ⇒ 변수 간 관계가 있는지 확인
        
### PCA
1. Factor Analysis → Communalities 0.5를 기준으로 변수를 걸러내는 작업
2. 변수를 걸러낸 이후 KMO 및 Bartlett 검정 진행
3. 선택된 변수로 Varimax, Equamax, Quartimax과 Principal Factor 기법을 사용해 Factor Analysis 진행
    - 상관행렬을 활용
    - Varimax의 결과가 비교적 깔끔하게 도출 : 다른 요인 회전 방식에 비해 상관성이 떨어지는 다양한 측면의 요인을 잘 도출해 우리의 분석 목적에 부합
    - Factor1-2 도출
        ![image](https://github.com/popper6508/202301bigdataanalysis/assets/118153199/c1eca5f7-f3f4-45cd-b57c-3781d44fdaac)

       
        
4. 도출된 Factor 기반으로 회귀분석 진행

- **기본적인 특성**
    - Factor1 특성
        ![image](https://github.com/popper6508/202301bigdataanalysis/assets/118153199/33555991-a113-47c8-b714-9a2a69230e22)


        - 수도권 및 광역시권과 나머지의 격차가 드러나는 지표 : 주로 인프라 혹은 생활 환경 여건 관련 지표로 볼 수 있다.
            - 수도권과 광역시권 주변은 어디든 상관없이 전반적으로 양호한 흐름
    - Factor2 특성
        ![image](https://github.com/popper6508/202301bigdataanalysis/assets/118153199/3e646eb7-86ee-43f6-9e1a-371498db5562)

  
        
        - 경기 남부 IT, 반도체 클러스터, 구미-창원-울산 중심의 제조업 클러스터 중심으로 양호한 흐름. 수도권 등 지역 내부에서도 다른 양상. 인프라와 생활 여건 관계없이 지역 내 부가가치 창출 수준에 따라 지표가 달라지는 흐름
            - 지역의 부가가치 창출 능력, 산업, 지역 내 인적자본 수준을 나타내는 지표로 해석 가능
    
    - factor1는 가까운 지역을 그대로 따라가기에 분포가 전반적으로 고르지만, factor는 지역군 내에서도 특정한 지역에 쏠리는 경향을 보이기에 대부분은 일정 수준 근방에 있지만 극단적으로 양호한 값을 보이는 지역 존재
    
    factor1는 가까운 지역을 그대로 따라가기에 분포가 전반적으로 고르지만, factor는 지역군 내에서도 특정한 지역에 쏠리는 경향을 보이기에 대부분은 일정 수준 근방에 있지만 극단적으로 양호한 값을 보이는 지역 존재
    
    - 전반적인 생활 인프라 여건과 경제 수준 중 무엇이 사람들의 투표 여부 의사결정에 큰 영향을 주는지 확인 가능.

- 주성분회귀
    - 확인한 포인트
        - 이분산 존재
        - 자기상관 존재
        - 적합도 : R^2
        - F-test : 결합 유의성
        - 독립변수 각각의 유의성 : t-test
