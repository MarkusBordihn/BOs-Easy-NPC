package de.markusbordihn.easynpc.client.screen.configuration.skin;

import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.MutableComponent;

public class EasyNPCButton extends Button {

    public EasyNPCButton(int x, int y, int width, int height, MutableComponent component, Button.OnPress onPress)
    {
        super(x,
                y,
                width,
                height,
                component,
                (button) -> onPress.onPress(button),
                (button) -> component);
    }

}
