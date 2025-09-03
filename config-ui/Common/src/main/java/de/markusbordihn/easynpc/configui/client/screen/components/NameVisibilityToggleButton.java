/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.client.screen.components;

import de.markusbordihn.easynpc.client.screen.components.MultiStateToggleButton;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.network.components.TextComponent;

public class NameVisibilityToggleButton extends MultiStateToggleButton {

  public static final int SPRITE_OFFSET_Y_NEVER = 166;
  public static final int SPRITE_OFFSET_Y_ALWAYS = 154;
  public static final int SPRITE_OFFSET_Y_NEAR = 178;
  public static final int SPRITE_OFFSET_X = 92;

  private NameVisibilityType currentVisibilityType;

  public NameVisibilityToggleButton(
      int left, int top, NameVisibilityType initialType, OnVisibilityChange onVisibilityChange) {
    this(left, top, DEFAULT_WIDTH, DEFAULT_HEIGHT, initialType, onVisibilityChange);
  }

  public NameVisibilityToggleButton(
      int left,
      int top,
      int width,
      int height,
      NameVisibilityType initialType,
      OnVisibilityChange onVisibilityChange) {
    super(
        left,
        top,
        width,
        height,
        createVisibilityStates(),
        getStateIndexFromType(initialType),
        (button, newStateIndex) -> {
          NameVisibilityToggleButton nameButton = (NameVisibilityToggleButton) button;
          nameButton.currentVisibilityType = getTypeFromStateIndex(newStateIndex);
          if (onVisibilityChange != null) {
            onVisibilityChange.onVisibilityChange(nameButton, nameButton.currentVisibilityType);
          }
        });

    this.currentVisibilityType = initialType;
  }

  private static ToggleState[] createVisibilityStates() {
    return new ToggleState[] {
      new ToggleState(
          SPRITE_OFFSET_X,
          SPRITE_OFFSET_Y_NEVER,
          TextComponent.getTranslatedConfigText("name_visibility.never")),
      new ToggleState(
          SPRITE_OFFSET_X,
          SPRITE_OFFSET_Y_ALWAYS,
          TextComponent.getTranslatedConfigText("name_visibility.always")),
      new ToggleState(
          SPRITE_OFFSET_X,
          SPRITE_OFFSET_Y_NEAR,
          TextComponent.getTranslatedConfigText("name_visibility.near")),
    };
  }

  private static int getStateIndexFromType(NameVisibilityType type) {
    return switch (type) {
      case NEVER -> 0;
      case ALWAYS -> 1;
      case NEAR -> 2;
    };
  }

  private static NameVisibilityType getTypeFromStateIndex(int stateIndex) {
    return switch (stateIndex) {
      case 0 -> NameVisibilityType.NEVER;
      case 1 -> NameVisibilityType.ALWAYS;
      case 2 -> NameVisibilityType.NEAR;
      default -> NameVisibilityType.ALWAYS;
    };
  }

  public NameVisibilityType getVisibilityType() {
    return this.currentVisibilityType;
  }

  public void setVisibilityType(NameVisibilityType type) {
    this.currentVisibilityType = type;
    this.setCurrentStateIndex(getStateIndexFromType(type));
  }

  @FunctionalInterface
  public interface OnVisibilityChange {
    void onVisibilityChange(NameVisibilityToggleButton button, NameVisibilityType newType);
  }
}
