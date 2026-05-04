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

package de.markusbordihn.easynpc.io;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.utils.SnbtFormatter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtAccounter;
import net.minecraft.nbt.NbtIo;
import net.minecraft.nbt.TagParser;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetFileHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private PresetFileHandler() {}

  public static CompoundTag load(File file) {
    if (file == null || !file.exists() || !file.isFile()) {
      log.error("Error loading preset file, file does not exist or is not a file: {}", file);
      return null;
    }

    String fileName = file.getName();
    PresetExportFormat format = PresetExportFormat.getPresetExportFormat(fileName);

    return switch (format) {
      case NBT -> loadNbt(file);
      case SNBT -> loadSnbt(file);
      case JSON -> {
        log.warn("JSON preset format is not supported yet for file: {}!", file);
        yield loadSnbt(file);
      }
      case UNKNOWN -> {
        log.error("Unknown preset file format for file: {}!", file);
        yield new CompoundTag();
      }
    };
  }

  public static CompoundTag loadNbt(File file) {
    if (file == null || !file.exists() || !file.isFile()) {
      log.error("Error loading NBT preset file, file does not exist or is not a file: {}", file);
      return null;
    }

    try {
      return NbtIo.readCompressed(file.toPath(), NbtAccounter.unlimitedHeap());
    } catch (IOException exception) {
      log.error("Failed to read NBT preset file {}:", file, exception);
      return null;
    }
  }

  public static CompoundTag loadSnbt(File file) {
    if (file == null || !file.exists() || !file.isFile()) {
      log.error("Error loading SNBT preset file, file does not exist or is not a file: {}", file);
      return null;
    }

    try {
      String content = Files.readString(file.toPath(), StandardCharsets.UTF_8);
      return TagParser.parseTag(content);
    } catch (IOException exception) {
      log.error("Failed to read SNBT preset file {}:", file, exception);
      return null;
    } catch (CommandSyntaxException exception) {
      log.error("Failed to parse SNBT preset file {}:", file, exception);
      return null;
    }
  }

  public static CompoundTag loadFromInputStream(
      InputStream inputStream, ResourceLocation resourceLocation) {
    if (inputStream == null) {
      log.error("Error loading preset, input stream is null for: {}", resourceLocation);
      return null;
    }

    PresetExportFormat format =
        PresetExportFormat.getPresetExportFormat(resourceLocation.getPath());

    try {
      if (format == PresetExportFormat.SNBT) {
        String content = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
        return TagParser.parseTag(content);
      } else if (format == PresetExportFormat.NBT) {
        return NbtIo.readCompressed(inputStream, NbtAccounter.unlimitedHeap());
      } else {
        log.error("Unknown preset format for: {}", resourceLocation);
        return null;
      }
    } catch (IOException exception) {
      log.error("Failed to read preset file {}:", resourceLocation, exception);
      return null;
    } catch (CommandSyntaxException exception) {
      log.error("Failed to parse SNBT preset file {}:", resourceLocation, exception);
      return null;
    }
  }

  public static boolean save(File file, CompoundTag compoundTag) {
    if (file == null || compoundTag == null || compoundTag.isEmpty()) {
      log.error("Error saving preset file {} with {} !", file, compoundTag);
      return false;
    }

    String fileName = file.getName();
    PresetExportFormat format = PresetExportFormat.getPresetExportFormat(fileName);

    if (format == PresetExportFormat.UNKNOWN) {
      log.warn("No preset extension found for file: {}, using default NBT format", file);
      return saveNbt(file, compoundTag);
    }

    if (format == PresetExportFormat.SNBT) {
      return saveSnbt(file, compoundTag);
    }

    return saveNbt(file, compoundTag);
  }

  public static boolean saveNbt(File file, CompoundTag compoundTag) {
    if (file == null || compoundTag == null || compoundTag.isEmpty()) {
      log.error("Error exporting NBT preset file {} with {} !", file, compoundTag);
      return false;
    }

    try {
      NbtIo.writeCompressed(compoundTag, file.toPath());
      return true;
    } catch (IOException exception) {
      log.error("Failed to export NBT preset file {} with {}:", file, compoundTag, exception);
      return false;
    }
  }

  public static boolean saveSnbt(File file, CompoundTag compoundTag) {
    if (file == null || compoundTag == null || compoundTag.isEmpty()) {
      log.error("Error exporting SNBT preset file {} with {} !", file, compoundTag);
      return false;
    }

    try (FileWriter writer = new FileWriter(file, StandardCharsets.UTF_8)) {
      writer.write(SnbtFormatter.format(compoundTag.toString()));
      return true;
    } catch (IOException exception) {
      log.error("Failed to export SNBT preset file {} with {}:", file, compoundTag, exception);
      return false;
    }
  }

  public static PresetMetadata extractMetadata(CompoundTag compoundTag) {
    if (compoundTag == null || compoundTag.isEmpty()) {
      return PresetMetadata.createDefault();
    }

    if (compoundTag.contains(PresetDataCapable.PRESET_METADATA_TAG)) {
      return PresetMetadata.fromCompoundTag(
          compoundTag.getCompound(PresetDataCapable.PRESET_METADATA_TAG));
    }

    return PresetMetadata.createDefault();
  }

  public static PresetMetadata extractMetadata(
      InputStream inputStream, ResourceLocation resourceLocation) {
    CompoundTag compoundTag = loadFromInputStream(inputStream, resourceLocation);
    return extractMetadata(compoundTag);
  }

  public static String getDisplayName(ResourceLocation resourceLocation, PresetMetadata metadata) {
    if (metadata == null || resourceLocation == null) {
      return PresetMetadata.DEFAULT_NAME;
    }

    if (PresetMetadata.DEFAULT_NAME.equals(metadata.name())) {
      String cleanFilename = PresetExportFormat.extractCleanFilename(resourceLocation.toString());
      return cleanFilename.isEmpty() ? metadata.name() : cleanFilename;
    }

    return metadata.name();
  }
}
