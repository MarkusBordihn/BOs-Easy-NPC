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
import net.minecraft.commands.synchronization.ArgumentTypeInfo;
import net.minecraft.commands.synchronization.ArgumentTypeInfos;
import net.minecraft.commands.synchronization.SingletonArgumentInfo;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

public class ModArgumentTypes {

  public static final DeferredRegister<ArgumentTypeInfo<?, ?>> COMMAND_ARGUMENT_TYPES =
      DeferredRegister.create(ForgeRegistries.COMMAND_ARGUMENT_TYPES, Constants.MOD_ID);

  public static final RegistryObject<SingletonArgumentInfo<DialogArgument>> DIALOG_ARGUMENT =
      COMMAND_ARGUMENT_TYPES.register(
          "dialog",
          () ->
              ArgumentTypeInfos.registerByClass(
                  DialogArgument.class, SingletonArgumentInfo.contextFree(DialogArgument::new)));
  public static final RegistryObject<SingletonArgumentInfo<EasyNPCArgument>> EASY_NPC_ARGUMENT =
      COMMAND_ARGUMENT_TYPES.register(
          "easy_npc",
          () ->
              ArgumentTypeInfos.registerByClass(
                  EasyNPCArgument.class, SingletonArgumentInfo.contextFree(EasyNPCArgument::new)));
  public static final RegistryObject<SingletonArgumentInfo<EntityTypeArgument>>
      ENTITY_TYPE_ARGUMENT =
          COMMAND_ARGUMENT_TYPES.register(
              "entity_type",
              () ->
                  ArgumentTypeInfos.registerByClass(
                      EntityTypeArgument.class,
                      SingletonArgumentInfo.contextFree(EntityTypeArgument::new)));
  public static final RegistryObject<SingletonArgumentInfo<EquipmentSlotArgument>>
      EQUIPMENT_SLOT_ARGUMENT =
          COMMAND_ARGUMENT_TYPES.register(
              "equipment_slot",
              () ->
                  ArgumentTypeInfos.registerByClass(
                      EquipmentSlotArgument.class,
                      SingletonArgumentInfo.contextFree(EquipmentSlotArgument::new)));

  private ModArgumentTypes() {}
}
