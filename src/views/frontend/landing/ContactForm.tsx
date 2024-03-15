"use client";

import { useMutation } from "@apollo/client";
import { yupResolver } from "@hookform/resolvers/yup";
import { useTranslations } from "next-intl";
import { useReCaptcha } from "next-recaptcha-v3";
import { useCallback } from "react";
import { Controller, useForm } from "react-hook-form";
import { toast } from "react-toastify";
import { Button, Col, FormFeedback, Input, Label, Row } from "reactstrap";
import * as yup from "yup";

import { EMAIL_REGEX } from "@/configs";
import { IS_DEVELOPMENT } from "@/environment";
import { CONTACT } from "@/graphql/contact";
import { ContactInput } from "@/types/contact";

type FormData = {
  name: string;
  email: string;
  message: string;
};

const defaultValues = IS_DEVELOPMENT
  ? {
      name: "Amanda Smith",
      email: "amandasmith@me.com",
      message: "I am interested in your product, please contact me.",
    }
  : {
      name: "",
      email: "",
      message: "",
    };

export const ContactForm = () => {
  const t = useTranslations("translations");

  const { executeRecaptcha } = useReCaptcha();

  const [contact, { loading }] = useMutation(CONTACT, {
    fetchPolicy: "no-cache",
  });

  const schema = yup.object().shape({
    name: yup
      .string()
      .trim()
      .required(t("propertyRequired", { property: t("fullName") })),
    email: yup
      .string()
      .email(t("propertyEmail", { property: t("email") }))
      .required(t("propertyRequired", { property: t("email") }))
      .matches(
        new RegExp(EMAIL_REGEX),
        t("propertyEmail", { property: t("email") })
      ),
    message: yup
      .string()
      .trim()
      .required(t("propertyRequired", { property: t("message") })),
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

  const onSubmit = useCallback(
    async (form: FormData) => {
      const recaptcha = await executeRecaptcha("form_submit");

      if (!recaptcha) {
        toast.error(t("reCaptchaError"));

        return;
      }

      const variables: ContactInput = {
        data: { ...form, subject: t("contact") },
      };

      await contact({
        variables,
        context: {
          headers: {
            recaptcha,
          },
        },
      })
        .then(({ data }) => {
          if (data?.sendContact) {
            toast.success(t("contactSuccess"));
          } else {
            toast.error(t("contactError"));
          }
        })
        .catch((error) => toast.error(error?.message ?? t("loginError")));
    },
    [contact, executeRecaptcha, t]
  );

  return (
    <form noValidate onSubmit={handleSubmit(onSubmit)}>
      <Row className="gx-5 mb-4">
        <Col md={6}>
          <Label className="text-dark mb-2" htmlFor="name">
            {t("fullName")}
          </Label>
          <Controller
            name="name"
            control={control}
            disabled={loading}
            rules={{ required: true }}
            render={({ field: { name, ...rest } }) => (
              <Input
                id={name}
                type="text"
                className="py-4"
                disabled={loading}
                placeholder={t("namePlaceholder")}
                invalid={Boolean(errors.name)}
                {...rest}
              />
            )}
          />
          <FormFeedback>{errors.name && errors.name.message}</FormFeedback>
        </Col>
        <Col md={6}>
          <Label className="text-dark mb-2" htmlFor="email">
            {t("email")}
          </Label>
          <Controller
            name="email"
            control={control}
            disabled={loading}
            rules={{ required: true }}
            render={({ field: { name, ...rest } }) => (
              <Input
                id={name}
                type="email"
                className=" py-4"
                disabled={loading}
                placeholder={t("emailPlaceholder")}
                invalid={Boolean(errors.email)}
                {...rest}
              />
            )}
          />
          <FormFeedback>{errors.email && errors.email.message}</FormFeedback>
        </Col>
      </Row>
      <div className="mb-4">
        <Label className="text-dark mb-2" htmlFor="message">
          {t("message")}
        </Label>
        <Controller
          name="message"
          control={control}
          disabled={loading}
          rules={{ required: true }}
          render={({ field: { name, ...rest } }) => (
            <Input
              id={name}
              type="textarea"
              className="py-3"
              disabled={loading}
              placeholder={t("messagePlaceholder")}
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
          disabled={loading}
          className="mt-4"
          type="submit"
        >
          {t("sendMessage")}
        </Button>
      </div>
    </form>
  );
};
