/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.screen.components;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.network.chat.Component;

public class UpDownButton extends AbstractWidget {

  protected final UpDownButton.OnUp onUp;
  protected final UpDownButton.OnDown onDown;
  private final Button upButton;
  private final Button downButton;

  public UpDownButton(int left, int top, int width, int height, OnUp onUp, OnDown onDown) {
    super(left, top, width, height, Component.literal(""));
    this.onUp = onUp;
    this.onDown = onDown;

    // Create up and down buttons
    int singleButtonHeight = height / 2;
    this.upButton =
        new SpriteButton(
            left,
            top,
            width,
            singleButtonHeight,
            Constants.TEXTURE_CONFIGURATION,
            5,
            2,
            93,
            59,
            9,
            6,
            button -> this.onUp.onUp(this));
    this.downButton =
        new SpriteButton(
            left,
            top + singleButtonHeight,
            width,
            singleButtonHeight,
            Constants.TEXTURE_CONFIGURATION,
            5,
            2,
            93,
            84,
            9,
            6,
            button -> this.onDown.onDown(this));
  }

  @Override
  public void updateNarration(NarrationElementOutput narrationElementOutput) {
    this.defaultButtonNarrationText(narrationElementOutput);
  }

  @Override
  public void render(PoseStack poseStack, int mouseX, int mouseY, float partialTicks) {
    if (this.upButton != null) {
      this.upButton.render(poseStack, mouseX, mouseY, partialTicks);
    }
    if (this.downButton != null) {
      this.downButton.render(poseStack, mouseX, mouseY, partialTicks);
    }
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    if (this.upButton != null && this.upButton.mouseClicked(mouseX, mouseY, button)) {
      return true;
    }
    return this.downButton != null && this.downButton.mouseClicked(mouseX, mouseY, button);
  }

  public void enableUpButton(boolean enable) {
    this.upButton.active = enable;
    this.upButton.visible = enable;
  }

  public void enableDownButton(boolean enable) {
    this.downButton.active = enable;
    this.downButton.visible = enable;
  }

  public interface OnUp {
    void onUp(UpDownButton button);
  }

  public interface OnDown {
    void onDown(UpDownButton button);
  }
}
