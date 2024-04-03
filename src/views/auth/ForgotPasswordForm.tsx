'use client'

import { yupResolver } from '@hookform/resolvers/yup'
import { useLocale, useTranslations } from 'next-intl'
import { useReCaptcha } from 'next-recaptcha-v3'
import { useState } from 'react'
import { Controller, useForm } from 'react-hook-form'
import { toast } from 'react-toastify'
import { Button, FormFeedback, FormGroup, Input, Label } from 'reactstrap'
import * as yup from 'yup'

import { CommonLogo } from '@/components/common/CommonLogo'
import { EMAIL_REGEX } from '@/configs'
import { api } from '@/configs/axios'
import { IS_DEVELOPMENT } from '@/environment'
import { Link, useRouter } from '@/navigation'
import { AuthFormProps } from '@/types/common'

type FormData = {
  email: string
}

const defaultValues = IS_DEVELOPMENT
  ? {
      email: 'amandasmith@me.com',
    }
  : {
      email: '',
    }

export const ForgotPasswordForm = ({ alignLogo }: AuthFormProps) => {
  const t = useTranslations()
  const locale = useLocale()
  const [disabled, setDisabled] = useState(false)
  const { executeRecaptcha } = useReCaptcha()

  const schema = yup.object().shape({
    email: yup
      .string()
      .email(t('propertyEmail', { property: t('email') }))
      .required(t('propertyRequired', { property: t('email') }))
      .matches(
        new RegExp(EMAIL_REGEX),
        t('propertyEmail', { property: t('email') })
      ),
  })

  const {
    control,
    handleSubmit,
    formState: { errors },
  } = useForm({
    defaultValues,
    mode: 'onBlur',
    resolver: yupResolver(schema),
  })

  const router = useRouter()

  const onSubmit = async (form: FormData) => {
    setDisabled(true)

    const data = {
      email: form.email ?? '',
    }

    const recaptcha = await executeRecaptcha('form_submit')

    if (!recaptcha) {
      toast.error(t('reCaptchaError'))

      return
    }

    await api
      .post(`/auth/forgot-password`, data, {
        headers: { recaptcha, locale },
      })
      .then(({ data }) => {
        if (data) {
          toast.success(t('forgotPasswordSuccess'))
          router.push('/auth/login')
        } else {
          toast.error(t('forgotPasswordError'))
        }
      })
      .catch((error) =>
        toast.error(error?.response?.data?.message ?? t('forgotPasswordError'))
      )

    setDisabled(false)
  }

  return (
    <div className="login-card login-dark">
      <div>
        <div>
          <CommonLogo alignLogo={alignLogo} />
        </div>
        <div className="login-main">
          <form
            className="theme-form"
            noValidate
            autoComplete="off"
            onSubmit={handleSubmit(onSubmit)}
          >
            <h4>{t('forgotPassword')}</h4>
            <p>{t('forgotPasswordInfo')}</p>
            <FormGroup>
              <Label htmlFor="email" className="col-form-label">
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
                    autoComplete="on"
                    placeholder={t('emailPlaceholder')}
                    invalid={Boolean(errors.email)}
                    {...rest}
                  />
                )}
              />
              <FormFeedback>
                {errors.email && errors.email.message}
              </FormFeedback>
            </FormGroup>
            <FormGroup className="mb-0 form-group">
              <div className="text-end mt-3">
                <Button
                  color="primary"
                  className="btn-block w-100"
                  type="submit"
                  disabled={disabled}
                >
                  {t('resetPassword')}
                </Button>
              </div>
              <p className="mt-4 mb-0 text-center">
                {t('rememberYourPassword')}
                <Link className="ms-2" href="/auth/login">
                  {t('signIn')}
                </Link>
              </p>
            </FormGroup>
          </form>
        </div>
      </div>
    </div>
  )
}
