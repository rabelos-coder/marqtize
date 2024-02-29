"use client";

import { useMutation } from "@apollo/client";
import { yupResolver } from "@hookform/resolvers/yup";
import { useTranslations } from "next-intl";
import { useEffect } from "react";
import { Controller, useForm } from "react-hook-form";
import { toast } from "react-toastify";
import { Button, FormFeedback, FormGroup, Input, Label } from "reactstrap";
import * as yup from "yup";

import { CommonLogo } from "@/components/common/CommonLogo";
import { EMAIL_REGEX } from "@/configs";
import { IS_DEVELOPMENT } from "@/environment";
import { FORGOT_PASSWORD } from "@/graphql/auth";
import { Link, useRouter } from "@/navigation";
import { AuthFormProps } from "@/types/common";

type FormData = {
  email: string;
};

const defaultValues = IS_DEVELOPMENT
  ? {
      email: "amandasmith@me.com",
    }
  : {
      email: "",
    };

export const ForgotPasswordForm = ({ alignLogo }: AuthFormProps) => {
  const [forgotPassword, { data, loading, error }] =
    useMutation(FORGOT_PASSWORD);

  const t = useTranslations("translations");

  const schema = yup.object().shape({
    email: yup
      .string()
      .email(t("invalidEmailFormat"))
      .required(t("invalidEmailRequired"))
      .matches(new RegExp(EMAIL_REGEX), t("invalidEmailFormat")),
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
    const callbackUrl = `${window.location.protocol}//${window.location.host}/auth/reset-password`;
    await forgotPassword({
      variables: {
        data: { email: form.email, callbackUrl },
      },
    });
    if (data?.forgotPassword) {
      toast.success(t("forgotPasswordSuccess"));
      router.push("/auth/login");
    } else {
      toast.error(t("forgotPasswordError"));
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
            <h4>{t("forgotPassword")}</h4>
            <p>{t("forgotPasswordInfo")}</p>
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
                {t("rememberYourPassword")}
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
