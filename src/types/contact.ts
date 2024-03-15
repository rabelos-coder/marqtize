export type ContactInput = {
  data: {
    name: string;
    subject: string;
    email: string;
    message: string;
    phone?: string;
    mobile?: string;
  };
};

export type SendContact = {
  sendContact: boolean;
};
