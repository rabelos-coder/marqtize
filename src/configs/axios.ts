import axios from 'axios'

const api = axios.create({
  baseURL: '/api',
  headers: {
    'Cache-Control': 'no-cache',
    Pragma: 'no-cache',
    Expires: '0',
  },
})

export default api
