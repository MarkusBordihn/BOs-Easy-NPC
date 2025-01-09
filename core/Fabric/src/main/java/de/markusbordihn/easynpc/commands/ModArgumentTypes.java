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

package de.markusbordihn.easynpc.commands;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.commands.arguments.DialogArgument;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.commands.arguments.EntityTypeArgument;
import de.markusbordihn.easynpc.commands.arguments.EquipmentSlotArgument;
import net.fabricmc.fabric.api.command.v2.ArgumentTypeRegistry;
import net.minecraft.commands.synchronization.SingletonArgumentInfo;
import net.minecraft.resources.ResourceLocation;

public class ModArgumentTypes {

  private ModArgumentTypes() {}

  public static void register() {
    ArgumentTypeRegistry.registerArgumentType(
        new ResourceLocation(Constants.MOD_ID, "dialog"),
        DialogArgument.class,
        SingletonArgumentInfo.contextFree(DialogArgument::new));
    ArgumentTypeRegistry.registerArgumentType(
        new ResourceLocation(Constants.MOD_ID, "easy_npc"),
        EasyNPCArgument.class,
        SingletonArgumentInfo.contextFree(EasyNPCArgument::new));
    ArgumentTypeRegistry.registerArgumentType(
        new ResourceLocation(Constants.MOD_ID, "entity_type"),
        EntityTypeArgument.class,
        SingletonArgumentInfo.contextFree(EntityTypeArgument::new));
    ArgumentTypeRegistry.registerArgumentType(
        new ResourceLocation(Constants.MOD_ID, "equipment_slot"),
        EquipmentSlotArgument.class,
        SingletonArgumentInfo.contextFree(EquipmentSlotArgument::new));
  }
}
