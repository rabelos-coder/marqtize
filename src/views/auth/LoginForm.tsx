'use client'

import { yupResolver } from '@hookform/resolvers/yup'
import { useLocale, useTranslations } from 'next-intl'
import { useReCaptcha } from 'next-recaptcha-v3'
import { useState } from 'react'
import { Controller, useForm } from 'react-hook-form'
import { FaFacebook, FaGoogle } from 'react-icons/fa'
import { toast } from 'react-toastify'
import { Button, FormFeedback, FormGroup, Input, Label } from 'reactstrap'
import * as yup from 'yup'

import CommonLogo from '@/components/common/CommonLogo'
import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from '@/configs'
import api from '@/configs/axios'
import { IS_DEVELOPMENT, SERVER_URL } from '@/environment'
import { useAppDispatch } from '@/hooks'
import { Link } from '@/navigation'
import { setAuth } from '@/store/slices/authSlice'

type AuthProps = {
  host?: string
  alignLogo?: string
}

type FormData = {
  email: string
  password: string
}

const defaultValues = IS_DEVELOPMENT
  ? {
      email: 'admin@example.com',
      password: '23277wyZ*',
    }
  : {
      email: '',
      password: '',
    }

export const LoginForm = ({ alignLogo }: AuthProps) => {
  const [disabled, setDisabled] = useState(false)
  const [rememberMe, setRememberMe] = useState(false)
  const [showPassword, setShowPassword] = useState(false)

  const t = useTranslations()
  const locale = useLocale()
  const dispatch = useAppDispatch()
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
    password: yup
      .string()
      .trim()
      .required(t('propertyRequired', { property: t('password') }))
      .matches(
        new RegExp(PASSWORD_STRENGTH_REGEX),
        t('propertyStrength', { property: t('password') })
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

  const onSubmit = async (form: FormData) => {
    setDisabled(true)

    const data = {
      email: form.email ?? '',
      password: form.password ?? '',
      rememberMe,
    }

    const recaptcha = await executeRecaptcha('form_submit')

    if (!recaptcha) {
      toast.error(t('reCaptchaError'))

      return
    }

    await api
      .post(`/auth/login`, data, { headers: { recaptcha, locale } })
      .then(({ data }) => {
        if (data) {
          const {
            user: { language },
          } = data
          toast.success(t('loginSuccess'))
          const locale = language.replace('_', '-').toLowerCase()
          dispatch(setAuth(data))

          window.location.href = `/${locale}/backend`
        } else {
          toast.error(t('loginError'))
        }
      })
      .catch((error) =>
        toast.error(error?.response?.data?.message ?? t('loginError'))
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
            <h4>{t('signInToAccount')}</h4>
            <p>{t('signInToAccountInfo')}</p>
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
            <FormGroup className="mb-0 form-group">
              <div className="checkbox p-0">
                <Input
                  id="rememberMe"
                  type="checkbox"
                  disabled={disabled}
                  onChange={() => setRememberMe(!rememberMe)}
                />
                <Label className="text-muted" for="rememberMe">
                  {t('rememberMe')}
                </Label>
              </div>
              <Link className="link" href="/auth/forgot-password">
                {t('forgotYourPassword')}
              </Link>
              <div className="text-end mt-3">
                <Button
                  color="primary"
                  className="btn-block w-100"
                  type="submit"
                  disabled={disabled}
                >
                  {t('signIn')}
                </Button>
              </div>
            </FormGroup>
            <h6 className="text-muted mt-4 or">{t('orSignInWith')}</h6>
            <div className="social mt-4">
              <div className="btn-showcase">
                <Button
                  color="light"
                  onClick={() =>
                    (window.location.href = `${SERVER_URL}/auth/facebook`)
                  }
                  disabled={disabled}
                  title={t('facebook')}
                >
                  <FaFacebook className="social-icon txt-facebook" />
                  {t('facebook')}
                </Button>
                <Button
                  color="light"
                  onClick={() =>
                    (window.location.href = `${SERVER_URL}/auth/google`)
                  }
                  disabled={disabled}
                  title={t('google')}
                >
                  <FaGoogle className="social-icon txt-google" />
                  {t('google')}
                </Button>
              </div>
            </div>
            <p className="mt-4 mb-0 text-center">
              {t('dontHaveAnAccount')}
              <Link className="ms-2" href="/auth/register">
                {t('createAccount')}
              </Link>
            </p>
          </form>
        </div>
      </div>
    </div>
  )
}
