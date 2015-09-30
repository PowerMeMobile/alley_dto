[![Build Status](https://travis-ci.org/PowerMeMobile/alley_dto.svg?branch=master)](https://travis-ci.org/PowerMeMobile/alley_dto)

# Alley DTO

This app is a set of DTO records that used by PowerAlley gateway
(kelly, funnel, just, soap_srv, oneapi_srv, mm_srv, email2sms).

The list of available DTO you can find in:

- common_dto.hrl
- funnel_dto.hrl
- just_dto.hrl

### Example

``` erlang
DTORecord = #funnel_ack_dto{},
{ok, Binary} = adto:encode(DTORecord),
{ok, DTORecord} = adto:decode(element(1, DTORecord), Binary).
```

More examples you can find in test directory.
