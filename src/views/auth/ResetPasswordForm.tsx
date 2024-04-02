'use client'

import { yupResolver } from '@hookform/resolvers/yup'
import axios from 'axios'
import { useSearchParams } from 'next/navigation'
import { useLocale, useTranslations } from 'next-intl'
import { useReCaptcha } from 'next-recaptcha-v3'
import { useState } from 'react'
import { Controller, useForm } from 'react-hook-form'
import { toast } from 'react-toastify'
import { Button, FormFeedback, FormGroup, Input, Label } from 'reactstrap'
import * as yup from 'yup'

import { CommonLogo } from '@/components/common/CommonLogo'
import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from '@/configs'
import { IS_DEVELOPMENT } from '@/environment'
import { Link, useRouter } from '@/navigation'
import { AuthFormProps } from '@/types/common'

type FormData = {
  email: string
  resetToken: string
  password: string
  passwordConfirmation: string
}

export const ResetPasswordForm = ({ alignLogo }: AuthFormProps) => {
  const [disabled, setDisabled] = useState(false)
  const [showPassword, setShowPassword] = useState(false)
  const [showPasswordConfirmation, setShowPasswordConfirmation] =
    useState(false)

  const t = useTranslations()
  const searchParams = useSearchParams()
  const locale = useLocale()
  const { executeRecaptcha } = useReCaptcha()
  const email = searchParams.get('email') ?? ''
  const resetToken = searchParams.get('token') ?? ''

  const defaultValues = IS_DEVELOPMENT
    ? {
        email,
        resetToken,
        password: 'abClK1@Xa',
        passwordConfirmation: 'abClK1@X',
      }
    : {
        email,
        resetToken,
        password: '',
        passwordConfirmation: '',
      }

  const schema = yup.object().shape({
    email: yup
      .string()
      .email(t('propertyEmail', { property: t('email') }))
      .required(t('propertyRequired', { property: t('email') }))
      .matches(
        new RegExp(EMAIL_REGEX),
        t('propertyEmail', { property: t('email') })
      ),
    resetToken: yup
      .string()
      .required(t('propertyRequired', { property: t('resetToken') })),
    password: yup
      .string()
      .trim()
      .required(t('propertyRequired', { property: t('password') }))
      .matches(
        new RegExp(PASSWORD_STRENGTH_REGEX),
        t('propertyStrength', { property: t('password') })
      ),
    passwordConfirmation: yup
      .string()
      .trim()
      .required(t('propertyRequired', { property: t('passwordConfirmation') }))
      .oneOf(
        [yup.ref('password')],
        t('propertyMatch', {
          property: t('passwordConfirmation'),
          match: t('password'),
        })
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
      resetToken: form.resetToken ?? '',
      password: form.password ?? '',
    }

    const recaptcha = await executeRecaptcha('form_submit')

    if (!recaptcha) {
      toast.error(t('reCaptchaError'))

      return
    }

    await axios
      .post(`/api/auth/reset-password`, data, {
        headers: { recaptcha, locale },
      })
      .then(({ data }) => {
        if (data) {
          toast.success(t('resetPasswordSuccess'))
          router.push('/auth/login')
        } else {
          toast.error(t('resetPasswordError'))
        }
      })
      .catch((error) =>
        toast.error(error?.response?.data?.message ?? t('resetPasswordError'))
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
            <h4>{t('resetPassword')}</h4>
            <p>{t('resetPasswordInfo')}</p>
            <FormGroup>
              <Label for="email" className="col-form-label required">
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
                    placeholder={t('emailPlaceholder')}
                    autoComplete="on"
                    invalid={Boolean(errors.email)}
                    {...rest}
                  />
                )}
              />
              <FormFeedback>
                {errors.email && errors.email.message}
              </FormFeedback>
            </FormGroup>
            <FormGroup>
              <Label for="resetToken" className="col-form-label required">
                {t('resetToken')}
              </Label>
              <Controller
                name="resetToken"
                control={control}
                disabled={disabled}
                rules={{ required: true }}
                render={({ field: { name, ...rest } }) => (
                  <Input
                    id={name}
                    autoComplete="on"
                    invalid={Boolean(errors.resetToken)}
                    {...rest}
                  />
                )}
              />
              <FormFeedback>
                {errors.resetToken && errors.resetToken.message}
              </FormFeedback>
            </FormGroup>
            <FormGroup>
              <Label for="password" className="col-form-label required">
                {t('password')}
              </Label>
              <div className="form-input position-relative">
                <Controller
                  name="password"
                  control={control}
                  disabled={disabled}
                  rules={{ required: true }}
                  render={({ field: { name, ...rest } }) => (
                    <Input
                      id={name}
                      type={showPassword ? 'text' : 'password'}
                      autoComplete="off"
                      invalid={Boolean(errors.password)}
                      {...rest}
                    />
                  )}
                />
                <FormFeedback>
                  {errors.password && errors.password.message}
                </FormFeedback>
                <div className="show-hide">
                  <span
                    onClick={() => setShowPassword(!showPassword)}
                    className={!showPassword ? 'show' : ''}
                  >
                    {showPassword ? t('hide') : t('show')}
                  </span>
                </div>
              </div>
            </FormGroup>
            <FormGroup>
              <Label
                for="passwordConfirmation"
                className="col-form-label required"
              >
                {t('passwordConfirmation')}
              </Label>
              <div className="form-input position-relative">
                <Controller
                  name="passwordConfirmation"
                  control={control}
                  disabled={disabled}
                  rules={{ required: true }}
                  render={({ field: { name, ...rest } }) => (
                    <Input
                      id={name}
                      type={showPasswordConfirmation ? 'text' : 'password'}
                      autoComplete="off"
                      invalid={Boolean(errors.passwordConfirmation)}
                      {...rest}
                    />
                  )}
                />
                <FormFeedback>
                  {errors.passwordConfirmation &&
                    errors.passwordConfirmation.message}
                </FormFeedback>
                <div className="show-hide">
                  <span
                    onClick={() =>
                      setShowPasswordConfirmation(!showPasswordConfirmation)
                    }
                    className={!showPasswordConfirmation ? 'show' : ''}
                  >
                    {showPasswordConfirmation ? t('hide') : t('show')}
                  </span>
                </div>
              </div>
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
                {t('alreadyPasswordYourPassword')}
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
