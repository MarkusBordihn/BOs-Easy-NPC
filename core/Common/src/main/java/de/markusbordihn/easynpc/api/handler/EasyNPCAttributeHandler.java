/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.api.handler;

import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributeType;
import de.markusbordihn.easynpc.data.attribute.NavigationType;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.world.entity.ai.attributes.Attribute;

public class EasyNPCAttributeHandler {

  private EasyNPCAttributeHandler() {}

  public static <T> boolean setDisplayAttribute(
      EasyNPC<?> easyNPC, DisplayAttributeType attributeType, T value) {
    return AttributeHandler.setDisplayAttribute(easyNPC, attributeType, value);
  }

  public static boolean setEnvironmentalAttribute(
      EasyNPC<?> easyNPC, EnvironmentalAttributeType attributeType, boolean value) {
    return AttributeHandler.setEnvironmentalAttribute(easyNPC, attributeType, value);
  }

  public static boolean setInteractionAttribute(
      EasyNPC<?> easyNPC, InteractionAttributeType attributeType, boolean value) {
    return AttributeHandler.setInteractionAttribute(easyNPC, attributeType, value);
  }

  public static boolean setMovementAttribute(
      EasyNPC<?> easyNPC, MovementAttributeType attributeType, boolean value) {
    return AttributeHandler.setMovementAttribute(easyNPC, attributeType, value);
  }

  public static boolean setMovementAttribute(
      EasyNPC<?> easyNPC, MovementAttributeType attributeType, double value) {
    return AttributeHandler.setMovementAttribute(easyNPC, attributeType, value);
  }

  public static boolean setCombatAttribute(
      EasyNPC<?> easyNPC, CombatAttributeType attributeType, boolean value) {
    return AttributeHandler.setCombatAttribute(easyNPC, attributeType, value);
  }

  public static boolean setCombatAttribute(
      EasyNPC<?> easyNPC, CombatAttributeType attributeType, double value) {
    return AttributeHandler.setCombatAttribute(easyNPC, attributeType, value);
  }

  public static boolean setBaseAttribute(EasyNPC<?> easyNPC, Attribute attribute, Double value) {
    return AttributeHandler.setBaseAttribute(easyNPC, attribute, value);
  }

  public static boolean setNavigationType(EasyNPC<?> easyNPC, NavigationType navigationType) {
    return AttributeHandler.setNavigationType(easyNPC, navigationType);
  }
}
