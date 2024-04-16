'use client'

import { yupResolver } from '@hookform/resolvers/yup'
import { useLocale, useTranslations } from 'next-intl'
import { useReCaptcha } from 'next-recaptcha-v3'
import { useCallback, useState } from 'react'
import { Controller, useForm } from 'react-hook-form'
import { toast } from 'react-toastify'
import { Button, Col, FormFeedback, Input, Label, Row } from 'reactstrap'
import * as yup from 'yup'

import { EMAIL_REGEX } from '@/configs'
import api from '@/configs/axios'
import { IS_DEVELOPMENT, IS_PRODUCTION } from '@/environment'

type FormData = {
  name: string
  email: string
  message: string
}

const defaultValues = IS_DEVELOPMENT
  ? {
      name: 'Amanda Smith',
      email: 'amandasmith@me.com',
      message: 'I am interested in your product, please contact me.',
    }
  : {
      name: '',
      email: '',
      message: '',
    }

export const ContactUs = () => {
  const t = useTranslations()
  const locale = useLocale()

  const [disabled, setDisabled] = useState(false)

  const { executeRecaptcha } = useReCaptcha()

  const schema = yup.object().shape({
    name: yup
      .string()
      .trim()
      .required(t('propertyRequired', { property: t('fullName') })),
    email: yup
      .string()
      .email(t('propertyEmail', { property: t('email') }))
      .required(t('propertyRequired', { property: t('email') }))
      .matches(
        new RegExp(EMAIL_REGEX),
        t('propertyEmail', { property: t('email') })
      ),
    message: yup
      .string()
      .trim()
      .required(t('propertyRequired', { property: t('message') })),
  })

  const {
    control,
    handleSubmit,
    formState: { errors },
    reset,
  } = useForm({
    defaultValues,
    mode: 'onBlur',
    resolver: yupResolver(schema),
  })

  const onSubmit = useCallback(
    async (form: FormData) => {
      setDisabled(true)

      const data = form

      const recaptcha = await executeRecaptcha('form_submit')

      if (!recaptcha) {
        toast.error(t('reCaptchaError'))

        return
      }

      await api
        .post('/contact', data, {
          headers: {
            locale,
            recaptcha,
          },
        })
        .then(({ data }) => {
          if (data) {
            toast.success(t('contactSuccess'))
            if (IS_PRODUCTION) reset()
          } else {
            toast.error(t('contactError'))
          }
        })
        .catch((error) => toast.error(error?.message ?? t('loginError')))

      setDisabled(false)
    },
    [executeRecaptcha, locale, reset, t]
  )

  return (
    <form noValidate onSubmit={handleSubmit(onSubmit)} autoComplete="on">
      <Row className="gx-5 mb-4">
        <Col md={6}>
          <Label className="text-dark mb-2" for="name">
            {t('fullName')}
          </Label>
          <Controller
            name="name"
            control={control}
            disabled={disabled}
            rules={{ required: true }}
            render={({ field: { name, ...rest } }) => (
              <Input
                id={name}
                type="text"
                className="py-4"
                disabled={disabled}
                placeholder={t('namePlaceholder')}
                invalid={Boolean(errors.name)}
                {...rest}
              />
            )}
          />
          <FormFeedback>{errors.name && errors.name.message}</FormFeedback>
        </Col>
        <Col md={6}>
          <Label className="text-dark mb-2" for="email">
            {t('email')}
          </Label>
          <Controller
            name="email"
            control={control}
            disabled={disabled}
            rules={{ required: true }}
            render={({ field: { name, ...rest } }) => (
              <Input
                id={name}
                type="email"
                className=" py-4"
                disabled={disabled}
                placeholder={t('emailPlaceholder')}
                invalid={Boolean(errors.email)}
                {...rest}
              />
            )}
          />
          <FormFeedback>{errors.email && errors.email.message}</FormFeedback>
        </Col>
      </Row>
      <div className="mb-4">
        <Label className="text-dark mb-2" for="message">
          {t('message')}
        </Label>
        <Controller
          name="message"
          control={control}
          disabled={disabled}
          rules={{ required: true }}
          render={({ field: { name, ...rest } }) => (
            <Input
              id={name}
              type="textarea"
              className="py-3"
              disabled={disabled}
              placeholder={t('messagePlaceholder')}
              rows={4}
              invalid={Boolean(errors.message)}
              {...rest}
            />
          )}
        />
        <FormFeedback>{errors.message && errors.message.message}</FormFeedback>
      </div>
      <div className="text-center">
        <Button
          color="primary"
          disabled={disabled}
          className="mt-4"
          type="submit"
        >
          {t('sendMessage')}
        </Button>
      </div>
    </form>
  )
}
