using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ScreenFreeze : MonoBehaviour
{
    public Material freezeMat;
    public float intensity;
    public float multiplier;
    bool trigger;

    // Start is called before the first frame update
    void Start()
    {
        freezeMat.SetFloat("_FreezeIntensity", intensity);
        intensity = 80f;
        trigger = false;
    }

    // Update is called once per frame
    void Update()
    {
        if(trigger && intensity > 2.5f)
        {
            intensity -= Time.deltaTime * multiplier;
            freezeMat.SetFloat("_FreezeIntensity", intensity);
        }
        if(!trigger && intensity < 80f)
        {
            intensity += Time.deltaTime * multiplier;
            freezeMat.SetFloat("_FreezeIntensity", intensity);
        }
    }

    private void OnTriggerEnter(Collider other)
    {
        if (other.gameObject.CompareTag("Player"))
        {
            trigger = true;
        }
    }

    private void OnTriggerExit(Collider other)
    {
        if (other.gameObject.CompareTag("Player"))
        {
            trigger = false;
        }
    }
}
