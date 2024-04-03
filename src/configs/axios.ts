import axios from 'axios'

export const api = axios.create({
  baseURL: '/api',
  headers: {
    'Cache-Control': 'no-cache',
    Pragma: 'no-cache',
    Expires: '0',
  },
})
