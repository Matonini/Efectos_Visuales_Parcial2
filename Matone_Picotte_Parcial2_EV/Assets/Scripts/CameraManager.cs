using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CameraManager : MonoBehaviour
{
    [SerializeField] float _mouseSens = 100f;       //Sensibilidad de la camara
    public Transform playerTransform;               //Referencia al transform del player

    float xRotation = 0f;                           //Rotación alrededor de X

    private void Start()
    {
        Cursor.lockState = CursorLockMode.Locked;   //Lockeo el mouse al centro de la pantalla y lo oculto
    }

    void Update()
    {
        RotateCamera();
    }

    public void RotateCamera()
    {
        //Inputs del mouse
        float mouseX = Input.GetAxis("Mouse X") * _mouseSens * Time.deltaTime;
        float mouseY = Input.GetAxis("Mouse Y") * _mouseSens * Time.deltaTime;

        xRotation -= mouseY;
        xRotation = Mathf.Clamp(xRotation, -20f, 20f);                      //Clampeo el valor para que no pueda darse vuelta la camara

        transform.localRotation = Quaternion.Euler(xRotation, 0f, 0f);      //Rotación en Y

        playerTransform.Rotate(Vector3.up * mouseX);                        //Rotación en X
    }
}
