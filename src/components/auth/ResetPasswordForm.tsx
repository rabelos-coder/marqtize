"use client";

import { useMutation } from "@apollo/client";
import { yupResolver } from "@hookform/resolvers/yup";
import { useTranslations } from "next-intl";
import { useEffect, useState } from "react";
import { Controller, useForm } from "react-hook-form";
import { toast } from "react-toastify";
import { Button, FormFeedback, FormGroup, Input, Label } from "reactstrap";
import * as yup from "yup";

import { CommonLogo } from "@/components/common/CommonLogo";
import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from "@/configs";
import { IS_DEVELOPMENT } from "@/environment";
import { RESET_PASSWORD } from "@/graphql/auth";
import { Link, useRouter } from "@/navigation";
import { AuthFormProps } from "@/types/common";

type FormData = {
  email: string;
  resetToken: string;
  password: string;
  passwordConfirmation: string;
};

const defaultValues = IS_DEVELOPMENT
  ? {
      email: "amandasmith@me.com",
      resetToken: "123456",
      password: "abClK1@Xa",
      passwordConfirmation: "abClK1@X",
    }
  : {
      email: "",
      resetToken: "",
      password: "",
      passwordConfirmation: "",
    };

export const ResetPasswordForm = ({ alignLogo }: AuthFormProps) => {
  const [showPassword, setShowPassword] = useState(false);
  const [showPasswordConfirmation, setShowPasswordConfirmation] =
    useState(false);
  const [resetPassword, { data, loading, error }] = useMutation(RESET_PASSWORD);

  const t = useTranslations("translations");

  const schema = yup.object().shape({
    email: yup
      .string()
      .email(t("invalidEmailFormat"))
      .required(t("invalidEmailRequired"))
      .matches(new RegExp(EMAIL_REGEX), t("invalidEmailFormat")),
    resetToken: yup.string().required(t("invalidResetTokenRequired")),
    password: yup
      .string()
      .trim()
      .required(t("invalidPasswordRequired"))
      .matches(
        new RegExp(PASSWORD_STRENGTH_REGEX),
        t("invalidPasswordStrength")
      ),
    passwordConfirmation: yup
      .string()
      .trim()
      .required(t("invalidPasswordConfirmationRequired"))
      .matches(
        new RegExp(PASSWORD_STRENGTH_REGEX),
        t("invalidPasswordStrength")
      )
      .oneOf(
        [yup.ref("password"), ""],
        t("invalidPasswordConfirmationMustMatch")
      ),
  });

  const {
    control,
    handleSubmit,
    formState: { errors },
  } = useForm({
    defaultValues,
    mode: "onBlur",
    resolver: yupResolver(schema),
  });

  const router = useRouter();

  const onSubmit = async (form: FormData) => {
    await resetPassword({
      variables: {
        data: {
          email: form.email,
          resetToken: form.resetToken,
          password: form.password,
        },
      },
    });
    if (data?.resetPassword) {
      toast.success(t("resetPasswordSuccess"));
      router.push("/auth/login");
    } else {
      toast.error(t("resetPasswordError"));
    }
  };

  useEffect(() => {
    if (error) toast.error(error.message);
  }, [error]);

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
            <h4>{t("resetPassword")}</h4>
            <p>{t("resetPasswordInfo")}</p>
            <FormGroup>
              <Label htmlFor="email" className="col-form-label">
                {t("email")}
              </Label>
              <Controller
                name="email"
                control={control}
                rules={{ required: true }}
                render={({
                  field: { name, value, onChange, onBlur, ...rest },
                }) => (
                  <Input
                    id={name}
                    type="email"
                    autoFocus
                    placeholder={t("emailPlaceholder")}
                    value={value}
                    onBlur={onBlur}
                    onChange={onChange}
                    valid={!Boolean(errors.email)}
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
              <Label htmlFor="resetToken" className="col-form-label">
                {t("resetToken")}
              </Label>
              <Controller
                name="resetToken"
                control={control}
                rules={{ required: true }}
                render={({
                  field: { name, value, onChange, onBlur, ...rest },
                }) => (
                  <Input
                    id={name}
                    autoFocus
                    value={value}
                    onBlur={onBlur}
                    onChange={onChange}
                    valid={!Boolean(errors.resetToken)}
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
              <Label htmlFor="password" className="col-form-label">
                {t("password")}
              </Label>
              <div className="form-input position-relative">
                <Controller
                  name="password"
                  control={control}
                  rules={{ required: true }}
                  render={({
                    field: { name, value, onChange, onBlur, ...rest },
                  }) => (
                    <Input
                      id={name}
                      type={showPassword ? "text" : "password"}
                      placeholder="*********"
                      value={value}
                      onBlur={onBlur}
                      onChange={onChange}
                      valid={!Boolean(errors.password)}
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
                    className={!showPassword ? "show" : ""}
                  >
                    {showPassword ? t("hide") : t("show")}
                  </span>
                </div>
              </div>
            </FormGroup>
            <FormGroup>
              <Label htmlFor="passwordConfirmation" className="col-form-label">
                {t("passwordConfirmation")}
              </Label>
              <div className="form-input position-relative">
                <Controller
                  name="passwordConfirmation"
                  control={control}
                  rules={{ required: true }}
                  render={({
                    field: { name, value, onChange, onBlur, ...rest },
                  }) => (
                    <Input
                      id={name}
                      type={showPasswordConfirmation ? "text" : "password"}
                      placeholder="*********"
                      value={value}
                      onBlur={onBlur}
                      onChange={onChange}
                      valid={!Boolean(errors.passwordConfirmation)}
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
                    className={!showPasswordConfirmation ? "show" : ""}
                  >
                    {showPasswordConfirmation ? t("hide") : t("show")}
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
                  disabled={loading}
                >
                  {t("resetPassword")}
                </Button>
              </div>
              <p className="mt-4 mb-0 text-center">
                {t("alreadyPasswordYourPassword")}
                <Link className="ms-2" href="/auth/login">
                  {t("signIn")}
                </Link>
              </p>
            </FormGroup>
          </form>
        </div>
      </div>
    </div>
  );
};
