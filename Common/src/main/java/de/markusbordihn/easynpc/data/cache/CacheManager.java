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

package de.markusbordihn.easynpc.data.cache;

import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.resources.ResourceLocation;

public class CacheManager {

  private static final HashMap<UUID, ActionEventSet> actionDataSetMap = new HashMap<>();
  private static final HashMap<UUID, DialogDataSet> dialogDataSetMap = new HashMap<>();
  private static final HashMap<UUID, ObjectiveDataSet> objectiveDataSetMap = new HashMap<>();
  private static final HashSet<ResourceLocation> customPresets = new HashSet<>();
  private static final HashSet<ResourceLocation> defaultPresets = new HashSet<>();
  private static final HashSet<ResourceLocation> worldPresets = new HashSet<>();

  private CacheManager() {
  }

  public static void clearCache() {
    actionDataSetMap.clear();
    dialogDataSetMap.clear();
    objectiveDataSetMap.clear();
    customPresets.clear();
    defaultPresets.clear();
    worldPresets.clear();
  }

  public static void clearActionDataSet(UUID uuid) {
    actionDataSetMap.remove(uuid);
  }

  public static void clearDialogDataSet(UUID uuid) {
    dialogDataSetMap.remove(uuid);
  }

  public static void clearObjectiveDataSet(UUID uuid) {
    objectiveDataSetMap.remove(uuid);
  }

  public static ActionEventSet getActionDataSet(UUID uuid) {
    return actionDataSetMap.get(uuid);
  }

  public static DialogDataSet getDialogDataSet(UUID uuid) {
    return dialogDataSetMap.get(uuid);
  }

  public static ObjectiveDataSet getObjectiveDataSet(UUID uuid) {
    return objectiveDataSetMap.get(uuid);
  }

  public static Set<ResourceLocation> getDefaultPresets() {
    return defaultPresets;
  }

  public static void setDefaultPresets(Set<ResourceLocation> presets) {
    defaultPresets.clear();
    defaultPresets.addAll(presets);
  }

  public static Set<ResourceLocation> getDefaultPresets(SkinModel skinModel) {
    String searchName = "/" + skinModel.getName() + "/";
    return defaultPresets.stream()
        .filter(resourceLocation -> resourceLocation.getPath().contains(searchName))
        .collect(Collectors.toSet());
  }

  public static boolean hasDefaultPresets() {
    return !defaultPresets.isEmpty();
  }

  public static Set<ResourceLocation> getCustomPresets() {
    return customPresets;
  }

  public static void setCustomPresets(Set<ResourceLocation> presets) {
    customPresets.clear();
    customPresets.addAll(presets);
  }

  public static Set<ResourceLocation> getCustomPresets(SkinModel skinModel) {
    String searchName = "/" + skinModel.getName() + "/";
    return customPresets.stream()
        .filter(resourceLocation -> resourceLocation.getPath().contains(searchName))
        .collect(Collectors.toSet());
  }

  public static boolean hasCustomPresets() {
    return !customPresets.isEmpty();
  }

  public static Set<ResourceLocation> getWorldPresets() {
    return worldPresets;
  }

  public static void setWorldPresets(Set<ResourceLocation> presets) {
    worldPresets.clear();
    worldPresets.addAll(presets);
  }

  public static Set<ResourceLocation> getWorldPresets(SkinModel skinModel) {
    String searchName = "/" + skinModel.getName() + "/";
    return worldPresets.stream()
        .filter(resourceLocation -> resourceLocation.getPath().contains(searchName))
        .collect(Collectors.toSet());
  }

  public static boolean hasWorldPresets() {
    return !worldPresets.isEmpty();
  }

  public static void setActionDataSet(UUID uuid, ActionEventSet actionDataSet) {
    actionDataSetMap.put(uuid, actionDataSet);
  }

  public static void setDialogDataSet(UUID uuid, DialogDataSet dialogDataSet) {
    dialogDataSetMap.put(uuid, dialogDataSet);
  }

  public static void setObjectiveDataSet(UUID uuid, ObjectiveDataSet objectiveDataSet) {
    objectiveDataSetMap.put(uuid, objectiveDataSet);
  }

  public static boolean hasActionDataSet(UUID uuid) {
    return actionDataSetMap.containsKey(uuid);
  }

  public static boolean hasDialogDataSet(UUID uuid) {
    return dialogDataSetMap.containsKey(uuid);
  }

  public static boolean hasObjectiveDataSet(UUID uuid) {
    return objectiveDataSetMap.containsKey(uuid);
  }

  public static boolean hasCacheData(UUID uuid) {
    return (!actionDataSetMap.isEmpty()
        || !dialogDataSetMap.isEmpty()
        || !objectiveDataSetMap.isEmpty())
        && (actionDataSetMap.containsKey(uuid)
        || dialogDataSetMap.containsKey(uuid)
        || objectiveDataSetMap.containsKey(uuid));
  }
}
