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

package de.markusbordihn.easynpc.configui.network.message;

import de.markusbordihn.easynpc.configui.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeCombatAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeDisplayAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeEntityAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeEntityBaseAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeEnvironmentalAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeInteractionAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeMovementAttributeMessage;
import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributeType;
import de.markusbordihn.easynpc.data.attribute.NavigationType;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import java.util.UUID;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.ai.attributes.Attribute;

public interface ServerAttributeNetworkMessageHandlerInterface {

  default void changeDisplayAttribute(
      UUID uuid, DisplayAttributeType displayAttributeType, Boolean booleanValue) {
    if (uuid != null && displayAttributeType != null && booleanValue != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeDisplayAttributeMessage(uuid, displayAttributeType, booleanValue));
    }
  }

  default void changeDisplayAttribute(
      UUID uuid, DisplayAttributeType displayAttributeType, Integer integerValue) {
    if (uuid != null && displayAttributeType != null && integerValue != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeDisplayAttributeMessage(uuid, displayAttributeType, integerValue));
    }
  }

  default void entityAttributeChange(
      UUID uuid, EntityAttribute entityAttribute, Boolean booleanValue) {
    if (uuid != null && entityAttribute != null && booleanValue != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeEntityAttributeMessage(uuid, entityAttribute, booleanValue));
    }
  }

  default void combatAttributeChange(
      UUID uuid, CombatAttributeType attributeType, Boolean booleanValue) {
    if (uuid != null && attributeType != null && booleanValue != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeCombatAttributeMessage(uuid, attributeType, booleanValue));
    }
  }

  default void combatAttributeChange(
      UUID uuid, CombatAttributeType attributeType, Double doubleValue) {
    if (uuid != null && attributeType != null && doubleValue != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeCombatAttributeMessage(uuid, attributeType, doubleValue));
    }
  }

  default void environmentalAttributeChange(
      UUID uuid, EnvironmentalAttributeType attributeType, Boolean booleanValue) {
    if (uuid != null && attributeType != null && booleanValue != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeEnvironmentalAttributeMessage(uuid, attributeType, booleanValue));
    }
  }

  default void interactionAttributeChange(
      UUID uuid, InteractionAttributeType attributeType, Boolean booleanValue) {
    if (uuid != null && attributeType != null && booleanValue != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeInteractionAttributeMessage(uuid, attributeType, booleanValue));
    }
  }

  default void movementAttributeChange(
      UUID uuid, MovementAttributeType attributeType, Boolean booleanValue) {
    if (uuid != null && attributeType != null && booleanValue != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeMovementAttributeMessage(uuid, attributeType, booleanValue));
    }
  }

  default void movementAttributeChange(
      UUID uuid, MovementAttributeType attributeType, Double doubleValue) {
    if (uuid != null && attributeType != null && doubleValue != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeMovementAttributeMessage(uuid, attributeType, doubleValue));
    }
  }

  default void navigationTypeChange(UUID uuid, NavigationType navigationType) {
    if (uuid != null && navigationType != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeMovementAttributeMessage(uuid, navigationType));
    }
  }

  default void entityBaseAttributeChange(UUID uuid, Attribute attribute, Double value) {
    if (uuid == null || attribute == null || value == null) {
      return;
    }

    if (BuiltInRegistries.ATTRIBUTE.getKey(attribute) == null) {
      return;
    }

    NetworkHandlerManager.sendMessageToServer(
        new ChangeEntityBaseAttributeMessage(uuid, attribute, Math.round(value * 100.0) / 100.0));
  }
}
